import { useState } from "react";
import Button from "./Button";

const Footer = () => {
  const [email, setEmail] = useState("");

  const links = {
    Product: ["Features", "Pricing", "Security", "Roadmap"],
    Company: ["About Us", "Careers", "Blog", "Press"],
    Support: ["Help Center", "Contact Us", "Privacy Policy", "Terms of Service"],
    Legal: ["Cookie Policy", "Licenses", "Refund Policy", "Compliance"],
  };

  return (
    <footer
      className="w-full pt-16 pb-8 px-6 border-t"
      style={{
        background: "hsl(0 0% 4%)",
        borderColor: "hsla(0,0%,100%,0.06)",
      }}
    >
      <div className="max-w-6xl mx-auto">
        <div className="grid grid-cols-2 lg:grid-cols-6 gap-8 sm:gap-10 mb-12">

          <div className="col-span-2 lg:col-span-2">
            <div className="flex items-center gap-2 mb-4">
              <div
                className="w-8 h-8 rounded-lg flex items-center justify-center font-bold text-black text-sm"
                style={{ background: "hsl(43 100% 55%)" }}
              >
                F
              </div>
              <span className="font-bold text-lg text-white">
                FluxPay
              </span>
            </div>

            <p className="text-sm text-gray-400 leading-relaxed mb-6">
              The future of digital payments. Fast, secure, and built for the next generation of finance.
            </p>

            <div className="flex flex-col sm:flex-row gap-2">
              <input
                type="email"
                placeholder="Enter your email"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                className="flex-1 px-3 py-2 text-xs rounded-lg text-white placeholder:text-gray-500 outline-none"
                style={{
                  background: "hsl(0 0% 10%)",
                  border: "1px solid hsl(0 0% 16%)",
                }}
              />
              <Button title="Subscribe" />
            </div>

            <p className="text-[10px] text-gray-500 mt-2">
              No spam. Unsubscribe anytime.
            </p>
          </div>

          {Object.entries(links).map(([category, items]) => (
            <div key={category} className="col-span-1">
              <h4 className="text-xs font-semibold text-white tracking-widest uppercase mb-4">
                {category}
              </h4>
              <ul className="space-y-2.5">
                {items.map((item) => (
                  <li key={item}>
                    <a
                      href="#"
                      className="text-sm text-gray-400 hover:text-yellow-400 transition-colors duration-200"
                    >
                      {item}
                    </a>
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </div>
        <div
          className="w-full h-px mb-8"
          style={{ background: "hsla(0,0%,100%,0.06)" }}
        />
        <div className="flex flex-col sm:flex-row items-center justify-between gap-4">

          <p className="text-xs text-gray-500 text-center sm:text-left">
            © {new Date().getFullYear()} FluxPay. All rights reserved.
          </p>
          <div className="flex items-center gap-3">
            {["Twitter", "Instagram", "LinkedIn", "GitHub"].map((social) => (
              <a
                key={social}
                href="#"
                aria-label={social}
                className="w-8 h-8 rounded-lg flex items-center justify-center text-gray-400 hover:text-yellow-400 transition-colors duration-200"
                style={{
                  background: "hsl(0 0% 10%)",
                  border: "1px solid hsl(0 0% 16%)",
                }}
              >
                {social[0]}
              </a>
            ))}
          </div>

          <div className="flex items-center gap-2">
            {["UPI", "VISA", "RuPay"].map((b) => (
              <span
                key={b}
                className="text-[10px] font-semibold px-2 py-1 rounded text-gray-400"
                style={{
                  background: "hsl(0 0% 10%)",
                  border: "1px solid hsl(0 0% 16%)",
                }}
              >
                {b}
              </span>
            ))}
          </div>
        </div>

      </div>
    </footer>
  );
};

export default Footer;