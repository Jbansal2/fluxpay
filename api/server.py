#!/usr/bin/env python3
"""
Thin HTTP adapter in front of the COBOL API gateway (APIGW).

All business logic stays in COBOL. This file only:
  * maps REST routes to the JSON operations APIGW understands,
  * passes the X-API-Key header through as "apikey",
  * runs `bin/runpgm APIGW` with the request on stdin,
  * turns APIGW's error codes into HTTP status codes.

Requests are handled one at a time (the indexed files are not built for
concurrent writers), which is what the lock below is for.

Routes
  GET  /api/health
  GET  /api/accounts/<account>/balance
  GET  /api/accounts/<account>/statement
  POST /api/transfers      {"from": "...", "to": "...", "amount": 100.0,
                            "narration": "..."}

Run:   python3 api/server.py [port]        (default 8080, from the repo root)
Try:   curl -H 'X-API-Key: k-read-0001' localhost:8080/api/accounts/1000000002/balance
"""
import json
import os
import re
import subprocess
import sys
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RUNPGM = os.path.join(ROOT, "bin", "runpgm")
LOCK = threading.Lock()

STATUS_FOR_CODE = {
    "UNAUTHORISED": 401,
    "FORBIDDEN": 403,
    "NOT_FOUND": 404,
    "BAD_REQUEST": 400,
    "LIMIT_EXCEEDED": 422,
    "INSUFFICIENT_FUNDS": 409,
    "ACCOUNT_BLOCKED": 409,
    "UNKNOWN_OP": 404,
}

ACCOUNT_ROUTE = re.compile(r"^/api/accounts/(\d{10})/(balance|statement)$")


def call_cobol(request: dict) -> dict:
    """Send one request line to APIGW and return its JSON response."""
    line = json.dumps(request, separators=(",", ":"))
    env = dict(os.environ, COB_LIBRARY_PATH=os.path.join(ROOT, "bin"))
    with LOCK:
        result = subprocess.run(
            [RUNPGM, "APIGW"], input=line + "\n", text=True,
            capture_output=True, cwd=ROOT, env=env, timeout=30)
    for out in result.stdout.splitlines():
        if out.startswith("{"):
            return json.loads(out)
    return {"status": "error", "code": "GATEWAY",
            "message": "no response from COBOL gateway"}


class Handler(BaseHTTPRequestHandler):
    server_version = "CoreBankAPI/1.0"

    def _send(self, body: dict):
        status = 200
        if body.get("status") != "ok":
            status = STATUS_FOR_CODE.get(body.get("code"), 500)
        data = json.dumps(body).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _key(self):
        return self.headers.get("X-API-Key", "")

    def do_GET(self):
        if self.path == "/api/health":
            return self._send(call_cobol({"op": "health"}))
        match = ACCOUNT_ROUTE.match(self.path)
        if not match:
            return self._send({"status": "error", "code": "NOT_FOUND",
                               "message": "no such route"})
        account, what = match.groups()
        op = "balance" if what == "balance" else "ministatement"
        self._send(call_cobol({"op": op, "apikey": self._key(),
                               "account": account}))

    def do_POST(self):
        if self.path != "/api/transfers":
            return self._send({"status": "error", "code": "NOT_FOUND",
                               "message": "no such route"})
        try:
            length = int(self.headers.get("Content-Length", 0))
            body = json.loads(self.rfile.read(length) or b"{}")
        except (ValueError, json.JSONDecodeError):
            return self._send({"status": "error", "code": "BAD_REQUEST",
                               "message": "body must be JSON"})
        request = {"op": "transfer", "apikey": self._key()}
        for field in ("from", "to", "amount", "narration"):
            if field in body:
                # the COBOL parser reads flat string/number values only
                value = body[field]
                request[field] = value if isinstance(value, (int, float)) \
                    else str(value).replace('"', "'")[:30]
        self._send(call_cobol(request))

    def log_message(self, fmt, *args):
        sys.stderr.write("api: " + fmt % args + "\n")


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8080
    print(f"CoreBank API on http://localhost:{port}/api (Ctrl-C to stop)")
    ThreadingHTTPServer(("", port), Handler).serve_forever()
