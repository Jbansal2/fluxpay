import React from "react";

const transactions = [
    {
        name: "For Airtel Prepaid",
        date: "Yesterday, 6:23 PM",
        amount: "₹100",
        icon: "🌀",
        color: "bg-red-500/20",
    },
    {
        name: "To Nike Apparels",
        date: "2 Feb",
        amount: "₹1000",
        icon: "👟",
        color: "bg-blue-500/20",
    },
    {
        name: "To BESCOM bill",
        date: "14 Jan",
        amount: "₹1000",
        icon: "⚡",
        color: "bg-yellow-500/20",
    },
    {
        name: "To Licious foods",
        date: "14 Jan",
        amount: "₹1000",
        icon: "🍗",
        color: "bg-purple-500/20",
    },
];

const features = [
    "Get your own Fluxpay card & UPI ID",
    "Enable quick payments with your fingerprint/Face ID",
    "Easily track, manage and budget your expenses",
    "Keep your bank statements clean & clutter-free",
];

const OneAccount = () => {
    return (
        <section
            className="w-full min-h-screen flex justify-center px-6 pt-24 pb-20"
            style={{ background: "hsl(0 0% 6%)" }}
        >
            <div className="max-w-6xl w-full flex flex-col md:flex-row items-start justify-between gap-20 text-center md:text-left">
                <div className="w-full md:w-1/3">
                    <div className="shrink-0 relative flex justify-center">
                        <div
                            className="relative w-[260px] h-[520px] rounded-[3rem] overflow-hidden"
                            style={{
                                background: "linear-gradient(180deg, hsl(0 0% 10%), hsl(0 0% 6%))",
                                border: "2px solid hsl(0 0% 20%)",
                                boxShadow: "0 40px 80px hsla(0,0%,0%,0.6), inset 0 1px 0 hsla(255,255%,255%,0.08)",
                            }}
                        >
                            <div className="absolute -right-[3px] top-28 w-[3px] h-10 rounded-r-sm" style={{ background: "hsl(0 0% 20%)" }} />
                            <div className="absolute -left-[3px] top-24 w-[3px] h-7 rounded-l-sm" style={{ background: "hsl(0 0% 20%)" }} />
                            <div className="absolute -left-[3px] top-36 w-[3px] h-7 rounded-l-sm" style={{ background: "hsl(0 0% 20%)" }} />

                            <div className="flex justify-center pt-3">
                                <div className="w-24 h-5 rounded-full flex items-center justify-center gap-2"
                                    style={{ background: "hsl(0 0% 4%)" }}>
                                    <div className="w-1.5 h-1.5 rounded-full" style={{ background: "hsl(0 0% 12%)" }} />
                                    <div className="w-2.5 h-2.5 rounded-full" style={{ background: "hsl(0 0% 12%)" }} />
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
                                    <div className="flex items-center gap-[1px]">
                                        <div className="w-5 h-2.5 rounded-[2px] relative" style={{ border: "1px solid hsla(255,255%,255%,0.3)" }}>
                                            <div className="absolute inset-[1.5px] rounded-[1px]" style={{ background: "hsl(120 60% 50%)", width: "70%" }} />
                                        </div>
                                    </div>
                                </div>
                            </div>
                            <div className="w-full h-px" style={{ background: "hsla(0,0%,100%,0.05)" }} />

                            <div className="px-5 pt-4">

                                <div className="flex justify-between items-center mb-4">
                                    <div className="w-7 h-7 rounded-full flex items-center justify-center"
                                        style={{ background: "hsl(0 0% 14%)", border: "1px solid hsl(0 0% 20%)" }}>
                                        <svg width="10" height="10" viewBox="0 0 10 10" fill="none">
                                            <path d="M7 2L3 5L7 8" stroke="hsl(0 0% 60%)" strokeWidth="1.2" strokeLinecap="round" />
                                        </svg>
                                    </div>
                                    <span className="text-xs font-bold text-foreground">All transactions</span>
                                    <div className="w-7 h-7 rounded-full flex items-center justify-center"
                                        style={{ background: "hsl(0 0% 14%)", border: "1px solid hsl(0 0% 20%)" }}>
                                        <svg width="10" height="10" viewBox="0 0 10 10" fill="none">
                                            <circle cx="5" cy="5" r="1" fill="hsl(0 0% 60%)" />
                                            <circle cx="5" cy="2" r="1" fill="hsl(0 0% 60%)" />
                                            <circle cx="5" cy="8" r="1" fill="hsl(0 0% 60%)" />
                                        </svg>
                                    </div>
                                </div>

                                <div className="flex gap-2 mb-5">
                                    {["Filter ▾", "Filter ▾", "Status ▾"].map((f, i) => (
                                        <div key={i} className="px-2.5 py-1 rounded-full text-[9px] text-muted-foreground"
                                            style={{ background: "hsl(0 0% 12%)", border: "1px solid hsl(0 0% 18%)" }}>
                                            {f}
                                        </div>
                                    ))}
                                </div>

                                <div className="text-[9px] font-semibold text-muted-foreground mb-3 tracking-widest uppercase">
                                    This month
                                </div>
                                <div className="space-y-1">
                                    {transactions.map((tx, i) => (
                                        <div
                                            key={i}
                                            className="flex items-center justify-between py-2.5 px-2 rounded-xl"
                                            style={{
                                                background: i === 0 ? "hsla(43,100%,55%,0.04)" : "transparent",
                                                borderBottom: "1px solid hsla(0,0%,100%,0.05)",
                                            }}
                                        >
                                            <div className="flex items-center gap-3">
                                                <div className={`w-9 h-9 rounded-full ${tx.color} flex items-center justify-center text-base shadow-sm`}>
                                                    {tx.icon}
                                                </div>
                                                <div>
                                                    <div className="text-[10px] font-semibold text-foreground">{tx.name}</div>
                                                    <div className="text-[9px] text-muted-foreground mt-0.5">{tx.date}</div>
                                                </div>
                                            </div>
                                            <div className="text-right">
                                                <span className="text-[11px] font-bold text-foreground">{tx.amount}</span>
                                                <div className="text-[8px] text-muted-foreground mt-0.5">Debited</div>
                                            </div>
                                        </div>
                                    ))}
                                </div>

                                <div className="text-[9px] font-semibold text-muted-foreground mt-4 tracking-widest uppercase">
                                    Last month
                                </div>
                            </div>
                        </div>

                        <div
                            className="absolute -bottom-6 left-1/2 -translate-x-1/2 w-40 h-8 rounded-full blur-2xl pointer-events-none"
                            style={{ background: "hsla(43,100%,55%,0.12)" }}
                        />
                    </div>
                </div>

                <div className="flex-1 max-w-2xl text-left">
                    <h2 className="text-4xl md:text-5xl font-bold font-display text-foreground leading-tight mb-10">
                        One account{" "}
                        <span className="text-primary">
                            for all your<br />spends
                        </span>
                    </h2>

                    <ul className="space-y-5">
                        {features.map((feature, i) => (
                            <li
                                key={i}
                                className="flex items-start gap-4"
                            >
                                <div className="w-5 h-5 rounded-full bg-primary/20 border border-primary/30 flex items-center justify-center shrink-0 mt-0.5">
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
                                <span className="text-base text-muted-foreground leading-relaxed">
                                    {feature}
                                </span>
                            </li>
                        ))}
                    </ul>
                </div>
            </div>
        </section>
    );
};

export default OneAccount;