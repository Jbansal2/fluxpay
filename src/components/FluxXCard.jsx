import React from "react";

const features = [
  "Numberless card that keeps your info safe",
  "Quick and secure payments with Tap & Pay",
  "Design your own card with doodles",
];

const FluxXCard = () => {
  return (
    <section className="w-full bg-[#1a1a1a] flex items-center justify-center px-6 py-10 relative overflow-hidden">
      <div className="max-w-6xl mx-auto w-full flex flex-col md:flex-row items-start justify-between gap-12">

        <div className="w-full md:w-1/2 text-left">
          <div className="flex items-baseline mb-6">
            <span className="text-5xl md:text-6xl font-black text-white tracking-tight">
              flux
            </span>
            <span className="text-5xl md:text-6xl font-black text-primary tracking-tight">
              X
            </span>
            <span className="text-5xl md:text-6xl font-black text-white tracking-tight">
              card
            </span>
          </div>

          <ul className="space-y-5">
            {features.map((f, i) => (
              <li key={i} className="flex items-start gap-4">
                <div
                  className="w-5 h-5 rounded-full flex items-center justify-center shrink-0 mt-1"
                  style={{
                    background: "hsla(43,100%,55%,0.15)",
                    border: "1px solid hsla(43,100%,55%,0.4)",
                  }}
                >
                  <svg width="10" height="8" viewBox="0 0 10 8" fill="none">
                    <path
                      d="M1 4L3.5 6.5L9 1"
                      stroke="hsl(43 100% 55%)"
                      strokeWidth="1.5"
                      strokeLinecap="round"
                      strokeLinejoin="round"
                    />
                  </svg>
                </div>
                <span className="text-base text-muted-foreground">
                  {f}
                </span>
              </li>
            ))}
          </ul>
        </div>

        <div className="w-full md:w-1/2 flex items-center justify-start md:justify-center">
          <div className="relative w-[320px] h-[260px]">
            <div
              className="absolute w-[280px] h-[170px] rounded-2xl overflow-hidden"
              style={{
                top: "8%",
                left: "8%",
                transform: "rotate(6deg)",
                background:
                  "linear-gradient(135deg, hsl(0 0% 10%), hsl(0 0% 7%))",
                border: "1px solid hsla(43,100%,55%,0.1)",
                boxShadow: "0 20px 60px hsla(0,0%,0%,0.5)",
                zIndex: 1,
              }}
            >
              <div
                className="absolute inset-0 opacity-[0.04]"
                style={{
                  backgroundImage:
                    "linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)",
                  backgroundSize: "28px 28px",
                }}
              />
            </div>

            <div
              className="absolute w-[280px] h-[170px] rounded-2xl overflow-hidden"
              style={{
                top: "30%",
                left: "0%",
                transform: "rotate(-3deg)",
                background:
                  "linear-gradient(135deg, hsl(0 0% 9%) 0%, hsl(30 30% 14%) 50%, hsl(0 0% 9%) 100%)",
                border: "1px solid hsla(43,60%,35%,0.35)",
                boxShadow:
                  "0 30px 80px hsla(0,0%,0%,0.6), 0 0 40px hsla(43,100%,55%,0.08)",
                zIndex: 2,
              }}
            >
              <div
                className="absolute inset-0 opacity-[0.06]"
                style={{
                  backgroundImage:
                    "linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)",
                  backgroundSize: "28px 28px",
                }}
              />

              <div className="absolute top-4 left-4 right-4 flex items-center justify-between">
                <div className="flex items-center gap-2">
                  <div
                    className="w-8 h-8 rounded-lg flex items-center justify-center font-black text-black text-sm"
                    style={{ background: "hsl(43 100% 55%)" }}
                  >
                    F
                  </div>
                  <span className="text-sm font-bold text-white">
                    FluxPay
                  </span>
                </div>
                <span className="text-white/80 font-black italic text-lg tracking-widest">
                  VISA
                </span>
              </div>

              <div className="absolute top-16 left-4">
                <div
                  className="w-9 h-7 rounded-md"
                  style={{
                    background:
                      "linear-gradient(135deg, hsl(43 100% 65%), hsl(43 80% 40%))",
                  }}
                />
              </div>

              <div className="absolute bottom-10 left-4">
                <div className="text-[8px] text-muted-foreground/60 tracking-[0.2em] mb-1">
                  CARD NUMBER
                </div>
                <div className="font-mono text-xs text-white/70 tracking-[0.18em]">
                  •••• •••• •••• 4289
                </div>
              </div>

              <div className="absolute bottom-3 left-4">
                <div className="text-[8px] text-muted-foreground/50 tracking-[0.15em] mb-0.5">
                  VALID
                </div>
                <div className="font-mono text-xs text-white/70">
                  12/28
                </div>
              </div>

            </div>
          </div>
        </div>

      </div>
    </section>
  );
};

export default FluxXCard;