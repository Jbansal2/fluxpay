const features = [
  "Make payments without a bank account",
  "Get a customised UPI & card as a teen",
  "Open for everyone ages 11+",
];

const FluxPayTeens = () => {
  return (
    <section
      className="w-full min-h-screen flex bg-[#1a1a1a] items-center px-6 py-20 relative overflow-hidden"
    >
      <div
        className="absolute top-1/2 right-1/4 -translate-y-1/2 w-[600px] h-[600px] rounded-full pointer-events-none"
        style={{ background: "radial-gradient(circle, hsla(43,100%,55%,0.04) 0%, transparent 70%)" }}
      />

      <div className="max-w-6xl mx-auto w-full flex flex-col-reverse md:flex-row items-center justify-center gap-14 md:gap-20">

        <div className="flex-1 max-w-lg mr-18">
          <h2 className="text-4xl md:text-5xl font-bold font-display leading-tight mb-8">
            <span className="text-primary">Your first and forever</span>
            <br />
            <span className="text-foreground">account</span>
          </h2>

          <ul className="space-y-5 mb-12">
            {features.map((f, i) => (
              <li key={i} className="flex items-center gap-4">
                <div
                  className="w-5 h-5 rounded-full flex items-center justify-center shrink-0"
                  style={{
                    background: "hsla(43,100%,55%,0.15)",
                    border: "1px solid hsla(43,100%,55%,0.4)",
                  }}
                >
                  <svg width="10" height="8" viewBox="0 0 10 8" fill="none">
                    <path d="M1 4L3.5 6.5L9 1" stroke="hsl(43 100% 55%)" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round" />
                  </svg>
                </div>
                <span className="text-base text-muted-foreground">{f}</span>
              </li>
            ))}
          </ul>
        </div>

        <div className="shrink-0 flex flex-col items-center gap-0">

          <div
            className="relative w-[260px] h-[520px] rounded-[3rem] overflow-hidden"
            style={{
              background: "linear-gradient(180deg, hsl(0 0% 10%), hsl(0 0% 6%))",
              border: "2px solid hsl(0 0% 20%)",
              boxShadow: "0 40px 80px hsla(0,0%,0%,0.7), inset 0 1px 0 hsla(255,255%,255%,0.08)",
            }}
          >
            <div className="absolute -right-[3px] top-28 w-[3px] h-10 rounded-r-sm" style={{ background: "hsl(0 0% 20%)" }} />
            <div className="absolute -left-[3px] top-24 w-[3px] h-7 rounded-l-sm" style={{ background: "hsl(0 0% 20%)" }} />
            <div className="absolute -left-[3px] top-36 w-[3px] h-7 rounded-l-sm" style={{ background: "hsl(0 0% 20%)" }} />

            <div className="flex justify-center pt-3">
              <div
                className="w-24 h-5 rounded-full flex items-center justify-center gap-2"
                style={{ background: "hsl(0 0% 4%)" }}
              >
                <div className="w-1.5 h-1.5 rounded-full" style={{ background: "hsl(0 0% 14%)" }} />
                <div className="w-2.5 h-2.5 rounded-full" style={{ background: "hsl(0 0% 14%)" }} />
              </div>
            </div>

            <div className="flex justify-between items-center px-5 pt-1 pb-2">
              <span className="text-[10px] font-semibold text-foreground">5:13 PM</span>
              <div className="flex items-center gap-1.5">
                <div className="flex items-end gap-[2px]">
                  {[3, 5, 7, 9].map((h, i) => (
                    <div key={i} className="w-[3px] rounded-[1px]"
                      style={{ height: `${h}px`, background: i < 3 ? "hsl(0 0% 60%)" : "hsl(0 0% 30%)" }} />
                  ))}
                </div>
                <svg width="12" height="9" viewBox="0 0 12 9" fill="none">
                  <path d="M1 4 Q6 0 11 4" stroke="hsl(0 0% 60%)" strokeWidth="1.2" strokeLinecap="round" fill="none" />
                  <path d="M3 6 Q6 4 9 6" stroke="hsl(0 0% 60%)" strokeWidth="1.2" strokeLinecap="round" fill="none" />
                  <circle cx="6" cy="8" r="0.8" fill="hsl(0 0% 60%)" />
                </svg>
                <div className="w-5 h-2.5 rounded-[2px] relative" style={{ border: "1px solid hsla(255,255%,255%,0.3)" }}>
                  <div className="absolute inset-[1.5px] rounded-[1px]" style={{ background: "hsl(120 60% 50%)", width: "70%" }} />
                </div>
              </div>
            </div>

            <div
              className="flex justify-between items-center px-5 pb-3"
              style={{ borderBottom: "1px solid hsla(0,0%,100%,0.06)" }}
            >
              <span className="text-muted-foreground text-sm">✕</span>
              <span className="text-xs font-bold text-foreground">
                flux<span className="text-primary">X</span>teens
              </span>
              <div className="w-4" />
            </div>

            <div className="px-4 pt-4 flex flex-col items-center gap-3">
              <div
                className="w-full rounded-2xl overflow-hidden relative"
                style={{ background: "linear-gradient(135deg, #c8a882, #e8c99a)", height: 200 }}
              >
                <div className="absolute inset-0 opacity-40">
                  <div className="absolute top-0 left-0 w-20 h-32 rounded-full"
                    style={{ background: "linear-gradient(135deg, #f97316, #fb923c)", filter: "blur(8px)" }} />
                  <div className="absolute bottom-0 right-0 w-24 h-28 rounded-full"
                    style={{ background: "linear-gradient(135deg, #3b82f6, #6366f1)", filter: "blur(10px)" }} />
                  <div className="absolute top-4 right-4 w-16 h-16 rounded-full"
                    style={{ background: "linear-gradient(135deg, #f59e0b, #fbbf24)", filter: "blur(6px)" }} />
                </div>

                <div
                  className="absolute top-3 left-1/2 -translate-x-1/2 px-3 py-1 rounded-full flex items-center gap-1.5 z-20"
                  style={{ background: "hsla(0,0%,0%,0.55)", backdropFilter: "blur(8px)" }}
                >
                  <span className="text-[9px] text-white font-medium whitespace-nowrap">UPI ID: 97264927845@flux</span>
                  <svg width="8" height="8" viewBox="0 0 10 10" fill="none">
                    <rect x="1" y="3" width="6" height="6" rx="1" stroke="white" strokeWidth="1" />
                    <path d="M3 3V2a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1H7" stroke="white" strokeWidth="1" />
                  </svg>
                </div>

                <div className="absolute inset-0 flex items-center justify-center z-10">
                  <div
                    className="w-28 h-28 rounded-xl p-2 relative"
                    style={{ background: "hsla(43,80%,75%,0.9)" }}
                  >
                    <svg width="96" height="96" viewBox="0 0 96 96" fill="none">
                      <rect x="4" y="4" width="24" height="24" rx="2" fill="none" stroke="#1a1a1a" strokeWidth="3" />
                      <rect x="9" y="9" width="14" height="14" rx="1" fill="#1a1a1a" />
                      <rect x="68" y="4" width="24" height="24" rx="2" fill="none" stroke="#1a1a1a" strokeWidth="3" />
                      <rect x="73" y="9" width="14" height="14" rx="1" fill="#1a1a1a" />
                      <rect x="4" y="68" width="24" height="24" rx="2" fill="none" stroke="#1a1a1a" strokeWidth="3" />
                      <rect x="9" y="73" width="14" height="14" rx="1" fill="#1a1a1a" />
                      {[
                        [36,4],[42,4],[48,4],[54,4],[36,10],[48,10],[54,10],[36,16],[42,16],
                        [36,22],[48,22],[36,28],[42,28],[54,28],
                        [4,36],[10,36],[22,36],[36,36],[48,36],[60,36],[68,36],[80,36],[92,36],
                        [4,42],[16,42],[28,42],[42,42],[60,42],[68,42],[80,42],
                        [4,48],[10,48],[22,48],[36,48],[48,48],[60,48],[80,48],[92,48],
                        [4,54],[16,54],[36,54],[48,54],[60,54],[68,54],[80,54],
                        [4,60],[10,60],[22,60],[36,60],[48,60],[60,60],[68,60],[80,60],[92,60],
                        [48,68],[60,68],[80,68],[42,74],[54,74],[68,74],[80,74],[92,74],
                        [48,80],[54,80],[68,80],[92,80],[42,86],[60,86],[68,86],[80,86],
                        [48,92],[60,92],[80,92],[92,92],
                      ].map(([x, y], i) => (
                        <rect key={i} x={x} y={y} width="4" height="4" rx="0.5" fill="#1a1a1a" />
                      ))}
                      <rect x="40" y="40" width="16" height="16" rx="3" fill="hsl(43 100% 55%)" />
                      <text x="48" y="52" textAnchor="middle" fill="#000" fontSize="9" fontWeight="800">F</text>
                    </svg>
                  </div>
                </div>

                <div className="absolute bottom-2 left-1/2 -translate-x-1/2 flex items-center gap-1 z-20">
                  <span className="text-[9px] font-bold text-white/80">flux</span>
                  <span className="text-[9px] font-bold text-primary">pay</span>
                  <span className="text-[8px] text-white/60">UPI</span>
                </div>
              </div>

              <div
                className="w-full flex items-center justify-between px-2 py-2 rounded-xl"
                style={{ background: "hsla(0,0%,100%,0.04)", border: "1px solid hsla(0,0%,100%,0.06)" }}
              >
                <span className="text-[10px] text-muted-foreground">Enter amount to receive</span>
                <svg width="14" height="14" viewBox="0 0 14 14" fill="none">
                  <path d="M5 3L9 7L5 11" stroke="hsl(0 0% 50%)" strokeWidth="1.2" strokeLinecap="round" />
                </svg>
              </div>

              <div className="w-full flex justify-around pt-1">
                {[
                  { icon: "✦", label: "Theme" },
                  { icon: "✏", label: "Edit" },
                  { icon: "⎋", label: "Share" },
                ].map((a) => (
                  <div key={a.label} className="flex flex-col items-center gap-1">
                    <div
                      className="w-8 h-8 rounded-full flex items-center justify-center text-xs text-muted-foreground"
                      style={{ background: "hsla(0,0%,100%,0.06)" }}
                    >
                      {a.icon}
                    </div>
                    <span className="text-[8px] text-muted-foreground">{a.label}</span>
                  </div>
                ))}
              </div>
            </div>
          </div>

          <div
            className="w-48 h-6 rounded-full blur-2xl pointer-events-none"
            style={{ background: "hsla(43,100%,55%,0.18)", marginTop: "-8px" }}
          />

          <div className="flex flex-col items-center mt-3">
            <div className="flex items-baseline">
              <span className="text-4xl font-black text-white font-display tracking-tight">flux</span>
              <span className="text-4xl font-black text-primary font-display tracking-tight">X</span>
              <span className="text-4xl font-black text-white font-display tracking-tight">teens</span>
            </div>
            <div className="flex items-center gap-1.5 mt-1">
              <span className="text-sm text-muted-foreground">by</span>
              <span className="text-sm font-bold text-white tracking-wide">flux</span>
              <div className="flex items-center">
                <span className="text-sm font-bold text-white">pay</span>
                <span className="flex gap-[3px] relative -top-1.5 ml-0.5">
                  <span className="w-1.5 h-1.5 rounded-full bg-orange-500 inline-block" />
                  <span className="w-1.5 h-1.5 rounded-full bg-green-500 inline-block" />
                </span>
              </div>
            </div>
          </div>

        </div>
      </div>
    </section>
  );
};

export default FluxPayTeens;