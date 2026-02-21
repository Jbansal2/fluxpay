import { useState, useRef } from "react";

const CreditCard = () => {
    const cardRef = useRef(null);
    const [tilt, setTilt] = useState({ x: 0, y: 0 });
    const [isHovering, setIsHovering] = useState(false);

    const handleMouseMove = (e) => {
        if (!cardRef.current) return;
        const rect = cardRef.current.getBoundingClientRect();
        const x = (e.clientX - rect.left) / rect.width - 0.5;
        const y = (e.clientY - rect.top) / rect.height - 0.5;
        setTilt({ x: y * -20, y: x * 20 });
    };

    const handleMouseLeave = () => {
        setIsHovering(false);
        setTilt({ x: 0, y: 0 });
    };

    return (
        <div className="relative animate-float mr-20">
            <div className="absolute -inset-4 rounded-3xl bg-primary/10 blur-2xl animate-pulse-slow" />
            <div
                className="absolute left-1/2 -translate-x-1/2 -top-20 w-[250px] h-[480px] rounded-[2rem] z-0 overflow-hidden"
                style={{
                    background: "linear-gradient(180deg, hsl(0 0% 10%), hsl(0 0% 6%))",
                    border: "1.5px solid hsla(255, 255%, 255%, 0.12)",
                    boxShadow: "0 0 60px hsla(0, 0%, 100%, 0.03), inset 0 1px 0 hsla(255,255%,255%,0.08)",
                }}
            >
                <div className="absolute -right-[3px] top-24 w-[3px] h-8 rounded-r-sm" style={{ background: "hsl(0 0% 14%)" }} />
                <div className="absolute -left-[3px] top-20 w-[3px] h-6 rounded-l-sm" style={{ background: "hsl(0 0% 14%)" }} />
                <div className="absolute -left-[3px] top-28 w-[3px] h-6 rounded-l-sm" style={{ background: "hsl(0 0% 14%)" }} />

                <div className="absolute inset-0 pointer-events-none rounded-[2rem]"
                    style={{ background: "radial-gradient(ellipse at 50% 0%, hsla(43,100%,55%,0.04) 0%, transparent 60%)" }} />

                <div className="mx-auto mt-3 w-20 h-5 rounded-full flex items-center justify-center gap-2"
                    style={{ background: "hsl(0 0% 4%)", border: "1px solid hsla(255,255%,255%,0.06)" }}>
                    <div className="w-2 h-2 rounded-full" style={{ background: "hsl(0 0% 8%)", border: "1px solid hsla(255,255%,255%,0.1)" }} />
                    <div className="w-3 h-3 rounded-full" style={{ background: "hsl(0 0% 8%)", border: "1px solid hsla(255,255%,255%,0.1)" }} />
                </div>

                <div className="px-4 mt-3 space-y-3">
                    <div className="flex justify-between items-center">
                        <span className="text-[9px] font-semibold text-foreground">9:41</span>
                        <div className="flex gap-1.5 items-center">
                            {/* Signal bars */}
                            <div className="flex items-end gap-[2px]">
                                {[3, 5, 7, 9].map((h, i) => (
                                    <div key={i} className="w-[3px] rounded-[1px]"
                                        style={{ height: `${h}px`, background: i < 3 ? "hsl(0 0% 60%)" : "hsl(0 0% 30%)" }} />
                                ))}
                            </div>
                            <svg width="10" height="8" viewBox="0 0 10 8" fill="none">
                                <path d="M1 3 Q5 0 9 3" stroke="hsl(0 0% 60%)" strokeWidth="1.2" strokeLinecap="round" fill="none" />
                                <path d="M2.5 5 Q5 3 7.5 5" stroke="hsl(0 0% 60%)" strokeWidth="1.2" strokeLinecap="round" fill="none" />
                                <circle cx="5" cy="7" r="0.8" fill="hsl(0 0% 60%)" />
                            </svg>
                            <div className="flex items-center gap-[1px]">
                                <div className="w-5 h-2.5 rounded-[2px] relative" style={{ border: "1px solid hsla(255,255%,255%,0.3)" }}>
                                    <div className="absolute inset-[1.5px] rounded-[1px]" style={{ background: "hsl(0 0% 60%)", width: "70%" }} />
                                </div>
                                <div className="w-[2px] h-1.5 rounded-r-[1px]" style={{ background: "hsla(255,255%,255%,0.3)" }} />
                            </div>
                        </div>
                    </div>

                    <div className="flex justify-between items-center">
                        <div>
                            <div className="text-[9px] text-muted-foreground">Welcome back 👋</div>
                            <div className="text-xs font-semibold text-foreground">Jitendra</div>
                        </div>
                        <div className="w-7 h-7 rounded-full flex items-center justify-center text-[9px] font-bold text-primary"
                            style={{ background: "hsla(43,100%,55%,0.12)", border: "1px solid hsla(43,100%,55%,0.2)" }}>
                            J
                        </div>
                    </div>

                    <div
                        className="rounded-xl p-3 relative overflow-hidden"
                        style={{
                            background: "linear-gradient(135deg, hsla(43, 100%, 55%, 0.08), hsla(43, 100%, 55%, 0.02))",
                            border: "1px solid hsla(43, 100%, 55%, 0.1)",
                        }}
                    >
                        <div className="absolute inset-0 opacity-[0.03] rounded-xl"
                            style={{
                                backgroundImage: `linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)`,
                                backgroundSize: "12px 12px",
                            }} />
                        <div className="relative z-10">
                            <div className="flex justify-between items-start">
                                <div>
                                    <div className="text-[9px] text-muted-foreground">Total Balance</div>
                                    <div className="text-sm font-bold text-foreground mt-0.5">₹48,520.80</div>
                                </div>
                                <div className="text-[8px] px-1.5 py-0.5 rounded-full text-emerald-400"
                                    style={{ background: "hsla(152,100%,50%,0.1)", border: "1px solid hsla(152,100%,50%,0.2)" }}>
                                    ↑ 2.4%
                                </div>
                            </div>
                            <svg width="100%" height="24" viewBox="0 0 200 24" className="mt-2">
                                <defs>
                                    <linearGradient id="sparkGrad" x1="0" y1="0" x2="0" y2="1">
                                        <stop offset="0%" stopColor="hsl(43,100%,55%)" stopOpacity="0.3" />
                                        <stop offset="100%" stopColor="hsl(43,100%,55%)" stopOpacity="0" />
                                    </linearGradient>
                                </defs>
                                <path d="M0 18 L30 14 L60 16 L90 10 L120 12 L150 6 L180 8 L200 4"
                                    stroke="hsl(43,100%,55%)" strokeWidth="1.5" fill="none" strokeLinecap="round" />
                                <path d="M0 18 L30 14 L60 16 L90 10 L120 12 L150 6 L180 8 L200 4 L200 24 L0 24Z"
                                    fill="url(#sparkGrad)" />
                            </svg>
                        </div>
                    </div>

                    <div className="flex justify-between gap-2">
                        {[
                            { label: "Send", icon: "↑" },
                            { label: "Receive", icon: "↓" },
                            { label: "Invest", icon: "%" },
                        ].map((a) => (
                            <div
                                key={a.label}
                                className="flex-1 flex flex-col items-center gap-1 py-2 rounded-xl"
                                style={{ background: "hsla(0, 0%, 100%, 0.03)", border: "1px solid hsla(0, 0%, 100%, 0.06)" }}
                            >
                                <div className="w-6 h-6 rounded-full flex items-center justify-center text-primary text-[10px] font-bold"
                                    style={{ background: "hsla(43,100%,55%,0.1)" }}>
                                    {a.icon}
                                </div>
                                <span className="text-[8px] text-muted-foreground">{a.label}</span>
                            </div>
                        ))}
                    </div>

                    <div>
                        <div className="flex justify-between items-center mb-2">
                            <div className="text-[9px] font-medium text-muted-foreground">Recent</div>
                            <div className="text-[8px] text-primary">See all</div>
                        </div>
                        {[
                            { name: "Netflix", amount: "-₹1,499", color: "bg-red-400/20", icon: "N" },
                            { name: "Salary", amount: "+₹52,000", color: "bg-emerald-400/20", icon: "S" },
                            { name: "Amazon", amount: "-₹890", color: "bg-amber-400/20", icon: "A" },
                        ].map((tx) => (
                            <div key={tx.name} className="flex items-center justify-between py-1.5"
                                style={{ borderBottom: "1px solid hsla(0,0%,100%,0.04)" }}>
                                <div className="flex items-center gap-2">
                                    <div className={`w-6 h-6 rounded-full ${tx.color} flex items-center justify-center`}>
                                        <span className="text-[8px] font-bold text-foreground/70">{tx.icon}</span>
                                    </div>
                                    <div>
                                        <div className="text-[9px] text-foreground">{tx.name}</div>
                                        <div className="text-[7px] text-muted-foreground">Today</div>
                                    </div>
                                </div>
                                <span className={`text-[9px] font-medium ${tx.amount.startsWith("+") ? "text-emerald-400" : "text-foreground"}`}>
                                    {tx.amount}
                                </span>
                            </div>
                        ))}
                    </div>

                </div>
            </div>

            <div
                ref={cardRef}
                onMouseMove={handleMouseMove}
                onMouseEnter={() => setIsHovering(true)}
                onMouseLeave={handleMouseLeave}
                className="
relative 
w-[160px] h-[240px]
sm:w-[190px] sm:h-[280px]
md:w-[210px] md:h-[310px]
rounded-2xl overflow-hidden 
glow-gold z-10 mt-10 cursor-pointer
translate-y-[30px]
md:-translate-x-[120px] md:translate-y-[50px]
"
                style={{
                    perspective: "1000px",
                    transform: `rotateX(${tilt.x}deg) rotateY(${tilt.y}deg) scale(${isHovering ? 1.03 : 1})`,
                    transition: isHovering ? "transform 0.1s ease-out" : "transform 0.5s ease-out",
                    transformStyle: "preserve-3d",
                }}
            >
                <div
                    className="absolute inset-0 z-20 pointer-events-none rounded-2xl"
                    style={{
                        background: isHovering
                            ? `radial-gradient(circle at ${(tilt.y / 20 + 0.5) * 100}% ${(-tilt.x / 20 + 0.5) * 100}%, hsla(43, 100%, 70%, 0.12), transparent 60%)`
                            : "none",
                        transition: "background 0.1s ease-out",
                    }}
                />
                <div
                    className="w-full h-full rounded-2xl flex flex-col justify-between relative overflow-hidden"
                    style={{
                        background: "linear-gradient(135deg, hsl(0 0% 12%), hsl(0 0% 8%))",
                        border: "1px solid hsla(43, 100%, 55%, 0.15)",
                    }}
                >
                    <div
                        className="absolute inset-0 opacity-[0.03]"
                        style={{
                            backgroundImage: `linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)`,
                            backgroundSize: "20px 20px",
                        }}
                    />

                    <div
                        className="absolute -top-8 -right-8 w-32 h-32 rounded-full pointer-events-none"
                        style={{ background: "radial-gradient(circle, hsla(43,100%,55%,0.08), transparent 70%)" }}
                    />

                    <div className="absolute inset-0 overflow-hidden rounded-2xl pointer-events-none">
                        <div
                            className="absolute w-[200%] h-[1px] opacity-10 rotate-[30deg] top-[40%] -left-[50%]"
                            style={{ background: "linear-gradient(90deg, transparent, hsl(43 100% 55%), transparent)" }}
                        />
                        <div
                            className="absolute w-[200%] h-[1px] opacity-[0.06] rotate-[30deg] top-[55%] -left-[50%]"
                            style={{ background: "linear-gradient(90deg, transparent, hsl(43 100% 55%), transparent)" }}
                        />
                    </div>

                    <div className="relative z-10 p-5 flex flex-col h-full justify-between">
                        <div className="flex items-center justify-between">
                            <div className="flex items-center gap-2">
                                <div className="w-8 h-8 rounded-lg bg-primary flex items-center justify-center">
                                    <span className="text-primary-foreground font-display font-bold text-xs">F</span>
                                </div>
                                <span className="font-display font-semibold text-sm text-foreground">FluxPay</span>
                            </div>
                            <svg width="36" height="24" viewBox="0 0 36 24" fill="none">
                                <circle cx="12" cy="12" r="10" fill="hsl(43 100% 55%)" fillOpacity="0.8" />
                                <circle cx="24" cy="12" r="10" fill="hsl(35 100% 45%)" fillOpacity="0.6" />
                            </svg>
                        </div>

                        <div className="mt-2">
                            <div
                                className="w-10 h-7 rounded-md relative overflow-hidden"
                                style={{ background: "linear-gradient(135deg, hsl(43 100% 65%), hsl(43 80% 45%))" }}
                            >
                                <div className="absolute inset-0 grid grid-cols-3 gap-[1px] p-[2px] opacity-40">
                                    {Array.from({ length: 6 }).map((_, i) => (
                                        <div key={i} className="bg-primary-foreground/30 rounded-[1px]" />
                                    ))}
                                </div>
                                <div className="absolute left-1/2 top-0 bottom-0 w-[1px] bg-black/20" />
                                <div className="absolute top-1/2 left-0 right-0 h-[1px] bg-black/20" />
                            </div>
                        </div>

                        <div className="flex justify-end -mt-6">
                            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" opacity="0.4">
                                <path d="M12 2 C12 2 20 6 20 12 C20 18 12 22 12 22" stroke="hsl(43 100% 55%)" strokeWidth="1.5" strokeLinecap="round" />
                                <path d="M12 6 C12 6 17 8.5 17 12 C17 15.5 12 18 12 18" stroke="hsl(43 100% 55%)" strokeWidth="1.5" strokeLinecap="round" />
                                <path d="M12 10 C12 10 14 11 14 12 C14 13 12 14 12 14" stroke="hsl(43 100% 55%)" strokeWidth="1.5" strokeLinecap="round" />
                            </svg>
                        </div>
                        <div>
                            <div className="text-xs text-muted-foreground mb-0.5 tracking-widest">CARD NUMBER</div>
                            <div className="font-mono text-sm text-foreground tracking-[0.2em]">•••• •••• •••• 4289</div>
                        </div>

                        <div className="flex items-end justify-between">
                            <div>
                                <div className="text-xs text-muted-foreground mb-0.5">VALID</div>
                                <div className="font-mono text-sm text-foreground">12/28</div>
                            </div>
                            <span className="text-foreground/70 font-black italic text-lg tracking-widest">VISA</span>
                        </div>

                    </div>
                </div>
            </div>
        </div>
    );
};

export default CreditCard;