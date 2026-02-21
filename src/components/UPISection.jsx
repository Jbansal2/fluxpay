import React from "react";

const features = [
    "Scan & Pay anywhere with your customised UPI ID",
    "Choose from a range of fun QR themes",
    "Recharge, pay bills and shop online, all within the app",
];

const floatingCards = [
    {
        top: "0%",
        right: "38%",
        bg: "radial-gradient(circle at 40% 40%, #1a3a2a, #0d1f16)",
        border: "hsla(120,60%,40%,0.3)",
    },
    {
        top: "8%",
        right: "2%",
        bg: "radial-gradient(circle at 60% 40%, #1a1a3a, #0d0d2e)",
        border: "hsla(260,60%,50%,0.3)",
    },
    {
        top: "35%",
        right: "28%",
        size: "w-36 h-36",
        bg: "radial-gradient(circle at 50% 50%, #1a1a3a, #0d0d2e)",
        border: "hsla(260,60%,50%,0.25)",
    },
    {
        top: "62%",
        right: "2%",
        bg: "radial-gradient(circle at 40% 60%, #0d2a1a, #0d1f10)",
        border: "hsla(120,60%,40%,0.3)",
    },
];

const UPISection = () => {
    return (
        <section
            className="w-full py-20 px-6 relative overflow-hidden"
            style={{ background: "hsl(0 0% 6%)" }}
        >
            <div
                className="absolute top-1/2 left-1/3 -translate-y-1/2 w-[500px] h-[500px] rounded-full pointer-events-none"
                style={{
                    background:
                        "radial-gradient(circle, hsla(43,100%,55%,0.03) 0%, transparent 70%)",
                }}
            />

            <div className="max-w-6xl mx-auto flex flex-col md:flex-row items-center gap-12 md:gap-16">

                <div className="flex-1 max-w-xl text-left">
                    <h2 className="text-4xl md:text-5xl font-bold font-display leading-tight mb-8 text-foreground">
                        UPI payments that go<br />
                        through in <span className="text-primary">seconds</span>
                    </h2>

                    <ul className="space-y-5 mb-10">
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
                                <span className="text-base text-muted-foreground">{f}</span>
                            </li>
                        ))}
                    </ul>

                    <button
                        className="px-8 py-4 rounded-full text-base font-bold text-black transition-all duration-200 hover:scale-105 active:scale-95"
                        style={{
                            background: "hsl(43 100% 55%)",
                            boxShadow: "0 8px 32px hsla(43,100%,55%,0.3)",
                        }}
                    >
                        Get your own UPI ID
                    </button>
                </div>

                <div className="flex-1 w-full max-w-md relative">

                    <div className="grid grid-cols-2 gap-6 md:hidden">
                        {floatingCards.map((card, i) => (
                            <div
                                key={i}
                                className={`${card.size || "w-full h-28"} rounded-full`}
                                style={{
                                    background: card.bg,
                                    border: `1.5px solid ${card.border}`,
                                    boxShadow: "0 8px 32px hsla(0,0%,0%,0.4)",
                                }}
                            />
                        ))}
                    </div>

                    <div className="hidden md:block relative h-[400px]">
                        {floatingCards.map((card, i) => (
                            <div
                                key={i}
                                className={`absolute ${card.size || "w-28 h-28"} rounded-full`}
                                style={{
                                    top: card.top,
                                    right: card.right,
                                    background: card.bg,
                                    border: `1.5px solid ${card.border}`,
                                    boxShadow: "0 8px 32px hsla(0,0%,0%,0.4)",
                                }}
                            />
                        ))}
                    </div>

                </div>
            </div>
        </section>
    );
};

export default UPISection;