import { useState } from "react";

const cards = [
    {
        id: 1,
        type: "VISA Platinum",
        name: "FluxPay Platinum",
        number: "•••• •••• •••• 4289",
        valid: "12/28",
        holder: "RAHUL SHARMA",
        network: "VISA",
        badge: "Most Popular",
        badgeBg: "hsl(43 100% 55%)",
        badgeColor: "black",
        gradient: "linear-gradient(135deg, hsl(0 0% 9%) 0%, hsl(30 30% 13%) 50%, hsl(0 0% 9%) 100%)",
        border: "hsla(43,100%,55%,0.3)",
        accent: "hsl(43 100% 55%)",
        accentDim: "hsla(43,100%,55%,0.1)",
        chipColor: "linear-gradient(135deg, hsl(43 100% 65%), hsl(43 80% 45%))",
        backBg: "linear-gradient(135deg, hsl(30 30% 10%), hsl(0 0% 7%))",
        features: ["5% cashback on all spends", "Zero forex markup", "1 Crore travel insurance", "Priority customer support", "Lounge access worldwide"],
        backInfo: { cvv: "***", bank: "FluxPay Bank Ltd.", limit: "₹5,00,000", type: "Signature" },
    },
    {
        id: 2,
        type: "RuPay Select",
        name: "FluxPay RuPay",
        number: "•••• •••• •••• 7731",
        valid: "09/27",
        holder: "PRIYA MEHTA",
        network: "RuPay",
        badge: "India First",
        badgeBg: "hsla(0,80%,55%,0.2)",
        badgeColor: "hsl(0 80% 70%)",
        gradient: "linear-gradient(135deg, hsl(0 0% 9%) 0%, hsl(0 20% 12%) 50%, hsl(0 0% 9%) 100%)",
        border: "hsla(0,80%,55%,0.3)",
        accent: "hsl(0 80% 60%)",
        accentDim: "hsla(0,80%,55%,0.1)",
        chipColor: "linear-gradient(135deg, hsl(0 80% 65%), hsl(0 60% 45%))",
        backBg: "linear-gradient(135deg, hsl(0 20% 10%), hsl(0 0% 7%))",
        features: ["Accepted at 5 Cr+ merchants", "UPI + card in one", "2% cashback on UPI pays", "Free NACH mandate", "Zero annual fee forever"],
        backInfo: { cvv: "***", bank: "FluxPay Bank Ltd.", limit: "₹2,00,000", type: "Select" },
    },
    {
        id: 3,
        type: "Teen Card",
        name: "FluxX Teens",
        number: "•••• •••• •••• 2024",
        valid: "03/29",
        holder: "ADITYA KUMAR",
        network: "RuPay",
        badge: "Ages 11+",
        badgeBg: "hsla(260,80%,60%,0.2)",
        badgeColor: "hsl(260 80% 75%)",
        gradient: "linear-gradient(135deg, hsl(0 0% 9%) 0%, hsl(260 20% 12%) 50%, hsl(0 0% 9%) 100%)",
        border: "hsla(260,80%,60%,0.3)",
        accent: "hsl(260 80% 70%)",
        accentDim: "hsla(260,80%,60%,0.1)",
        chipColor: "linear-gradient(135deg, hsl(260 80% 70%), hsl(260 60% 50%))",
        backBg: "linear-gradient(135deg, hsl(260 20% 10%), hsl(0 0% 7%))",
        features: ["No bank account needed", "Parental spend controls", "Custom doodle design", "Pocket money management", "Savings goals tracker"],
        backInfo: { cvv: "***", bank: "FluxPay Bank Ltd.", limit: "₹10,000", type: "Prepaid" },
    },
    {
        id: 4,
        type: "Shopping Card",
        name: "FluxPay Shop",
        number: "•••• •••• •••• 9981",
        valid: "06/28",
        holder: "SNEHA PATEL",
        network: "VISA",
        badge: "Best for Shopping",
        badgeBg: "hsla(200,100%,50%,0.2)",
        badgeColor: "hsl(200 100% 65%)",
        gradient: "linear-gradient(135deg, hsl(0 0% 9%) 0%, hsl(200 20% 12%) 50%, hsl(0 0% 9%) 100%)",
        border: "hsla(200,100%,50%,0.3)",
        accent: "hsl(200 100% 60%)",
        accentDim: "hsla(200,100%,50%,0.1)",
        chipColor: "linear-gradient(135deg, hsl(200 100% 65%), hsl(200 80% 45%))",
        backBg: "linear-gradient(135deg, hsl(200 20% 10%), hsl(0 0% 7%))",
        features: ["10% off on Amazon & Flipkart", "1.5x reward points online", "EMI on 0% interest", "Free delivery memberships", "Price drop alerts"],
        backInfo: { cvv: "***", bank: "FluxPay Bank Ltd.", limit: "₹3,00,000", type: "Shopping" },
    },
    {
        id: 5,
        type: "Prepaid Card",
        name: "FluxPay Go",
        number: "•••• •••• •••• 5544",
        valid: "11/26",
        holder: "RAHUL VERMA",
        network: "VISA",
        badge: "Instant Activate",
        badgeBg: "hsla(120,60%,40%,0.2)",
        badgeColor: "hsl(120 60% 60%)",
        gradient: "linear-gradient(135deg, hsl(0 0% 9%) 0%, hsl(120 15% 11%) 50%, hsl(0 0% 9%) 100%)",
        border: "hsla(120,60%,40%,0.3)",
        accent: "hsl(120 60% 55%)",
        accentDim: "hsla(120,60%,40%,0.1)",
        chipColor: "linear-gradient(135deg, hsl(120 60% 60%), hsl(120 50% 40%))",
        backBg: "linear-gradient(135deg, hsl(120 15% 9%), hsl(0 0% 7%))",
        features: ["Load & spend instantly", "No KYC for ₹10,000 limit", "Works offline via NFC", "Gift it to anyone", "Auto-reload from bank"],
        backInfo: { cvv: "***", bank: "FluxPay Bank Ltd.", limit: "₹10,000", type: "Prepaid" },
    },
    {
        id: 6,
        type: "Business Card",
        name: "FluxPay Biz",
        number: "•••• •••• •••• 3310",
        valid: "08/29",
        holder: "FLUX TECH PVT LTD",
        network: "VISA",
        badge: "For Business",
        badgeBg: "hsla(30,100%,50%,0.2)",
        badgeColor: "hsl(30 100% 65%)",
        gradient: "linear-gradient(135deg, hsl(0 0% 9%) 0%, hsl(30 20% 12%) 50%, hsl(0 0% 9%) 100%)",
        border: "hsla(30,100%,50%,0.3)",
        accent: "hsl(30 100% 60%)",
        accentDim: "hsla(30,100%,50%,0.1)",
        chipColor: "linear-gradient(135deg, hsl(30 100% 65%), hsl(30 80% 45%))",
        backBg: "linear-gradient(135deg, hsl(30 20% 10%), hsl(0 0% 7%))",
        features: ["GST invoice auto-generation", "Multi-user access up to 10", "Expense analytics dashboard", "Higher spending limits", "Dedicated relationship manager"],
        backInfo: { cvv: "***", bank: "FluxPay Bank Ltd.", limit: "₹25,00,000", type: "Corporate" },
    },
];

const NetworkBadge = ({ network, accent }) => {
    if (network === "VISA") {
        return <span className="font-black italic text-base tracking-widest text-white/80">VISA</span>;
    }
    return (
        <div className="flex items-center gap-1">
            <span className="font-black italic text-sm text-white/80">RuPay</span>
            <svg width="14" height="14" viewBox="0 0 12 12" fill="none">
                <polygon points="6,1 11,6 6,11 1,6" fill={accent} />
                <polygon points="9,1 12,6 9,11 6,6" fill={accent} opacity="0.6" />
            </svg>
        </div>
    );
};

const FlipCard = ({ card }) => {
    const [flipped, setFlipped] = useState(false);

    return (
        <div
            className="relative w-full cursor-pointer"
            style={{ height: 180, perspective: "1000px" }}
            onMouseEnter={() => setFlipped(true)}
            onMouseLeave={() => setFlipped(false)}
        >
            <div
                className="relative w-full h-full"
                style={{
                    transformStyle: "preserve-3d",
                    transform: flipped ? "rotateY(180deg)" : "rotateY(0deg)",
                    transition: "transform 0.6s cubic-bezier(0.4, 0, 0.2, 1)",
                }}
            >
                {/* ── Front ── */}
                <div
                    className="absolute inset-0 rounded-2xl overflow-hidden"
                    style={{
                        backfaceVisibility: "hidden",
                        WebkitBackfaceVisibility: "hidden",
                        background: card.gradient,
                        border: `1px solid ${card.border}`,
                        boxShadow: `0 8px 32px hsla(0,0%,0%,0.5)`,
                    }}
                >
                    {/* Grid */}
                    <div className="absolute inset-0 opacity-[0.04]"
                        style={{ backgroundImage: `linear-gradient(${card.accent} 1px, transparent 1px), linear-gradient(90deg, ${card.accent} 1px, transparent 1px)`, backgroundSize: "24px 24px" }} />
                    {/* Sheen */}
                    <div className="absolute inset-0" style={{ background: "linear-gradient(135deg, transparent 30%, hsla(255,255%,255%,0.03) 50%, transparent 70%)" }} />
                    {/* Glow */}
                    <div className="absolute -top-10 -right-10 w-32 h-32 rounded-full pointer-events-none"
                        style={{ background: `radial-gradient(circle, ${card.accentDim.replace("0.1", "0.2")}, transparent 70%)` }} />

                    <div className="relative z-10 p-4 flex flex-col h-full justify-between">
                        <div className="flex items-center justify-between">
                            <div className="flex items-center gap-2">
                                <div className="w-7 h-7 rounded-lg flex items-center justify-center font-black text-black text-xs"
                                    style={{ background: card.accent }}>F</div>
                                <span className="text-xs font-bold text-white">{card.name}</span>
                            </div>
                            <NetworkBadge network={card.network} accent={card.accent} />
                        </div>

                        <div className="flex items-center justify-between">
                            <div className="w-9 h-6 rounded-md relative overflow-hidden" style={{ background: card.chipColor }}>
                                <div className="absolute inset-0 grid grid-cols-3 gap-[1px] p-[1.5px] opacity-40">
                                    {Array.from({ length: 6 }).map((_, i) => <div key={i} className="bg-black/30 rounded-[1px]" />)}
                                </div>
                                <div className="absolute left-1/2 top-0 bottom-0 w-[1px] bg-black/20" />
                                <div className="absolute top-1/2 left-0 right-0 h-[1px] bg-black/20" />
                            </div>
                            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" opacity="0.4">
                                <path d="M12 2C12 2 20 6 20 12C20 18 12 22 12 22" stroke={card.accent} strokeWidth="1.5" strokeLinecap="round" />
                                <path d="M12 6C12 6 17 8.5 17 12C17 15.5 12 18 12 18" stroke={card.accent} strokeWidth="1.5" strokeLinecap="round" />
                                <path d="M12 10C12 10 14 11 14 12C14 13 12 14 12 14" stroke={card.accent} strokeWidth="1.5" strokeLinecap="round" />
                            </svg>
                        </div>

                        <div>
                            <div className="text-[8px] text-white/30 tracking-[0.15em] mb-0.5">CARD NUMBER</div>
                            <div className="font-mono text-[11px] text-white/70 tracking-[0.15em]">{card.number}</div>
                        </div>

                        <div className="flex items-end justify-between">
                            <div>
                                <div className="text-[7px] text-white/30 tracking-widest mb-0.5">VALID THRU</div>
                                <div className="font-mono text-[10px] text-white/70">{card.valid}</div>
                            </div>
                            <div className="text-right">
                                <div className="text-[7px] text-white/30 tracking-widest mb-0.5">CARDHOLDER</div>
                                <div className="text-[8px] text-white/60 font-medium tracking-wide">{card.holder}</div>
                            </div>
                        </div>
                    </div>
                </div>

                {/* ── Back ── */}
                <div
                    className="absolute inset-0 rounded-2xl overflow-hidden"
                    style={{
                        backfaceVisibility: "hidden",
                        WebkitBackfaceVisibility: "hidden",
                        transform: "rotateY(180deg)",
                        background: card.backBg,
                        border: `1px solid ${card.border}`,
                        boxShadow: `0 8px 32px hsla(0,0%,0%,0.5)`,
                    }}
                >
                    {/* Magnetic strip */}
                    <div className="w-full h-10 mt-6" style={{ background: "hsl(0 0% 5%)" }} />

                    {/* Signature + CVV strip */}
                    <div className="px-4 mt-4 flex items-center gap-3">
                        <div className="flex-1 h-8 rounded-md flex items-center px-3"
                            style={{ background: "repeating-linear-gradient(45deg, hsl(0 0% 85%), hsl(0 0% 85%) 2px, hsl(0 0% 90%) 2px, hsl(0 0% 90%) 8px)" }}>
                            <span className="text-[8px] text-gray-500 italic font-medium">{card.holder}</span>
                        </div>
                        <div className="px-3 py-1 rounded text-xs font-mono font-bold text-white"
                            style={{ background: "hsl(0 0% 14%)", border: `1px solid ${card.border}` }}>
                            {card.backInfo.cvv}
                        </div>
                    </div>
                    <div className="px-4 mt-0.5">
                        <span className="text-[8px] text-white/20">CVV</span>
                    </div>

                    {/* Card info */}
                    <div className="px-4 mt-3 grid grid-cols-2 gap-2">
                        <div className="rounded-lg p-2" style={{ background: "hsla(0,0%,100%,0.03)", border: `1px solid ${card.border}` }}>
                            <div className="text-[7px] text-white/30 mb-0.5">CREDIT LIMIT</div>
                            <div className="text-[10px] font-bold" style={{ color: card.accent }}>{card.backInfo.limit}</div>
                        </div>
                        <div className="rounded-lg p-2" style={{ background: "hsla(0,0%,100%,0.03)", border: `1px solid ${card.border}` }}>
                            <div className="text-[7px] text-white/30 mb-0.5">CARD TYPE</div>
                            <div className="text-[10px] font-bold" style={{ color: card.accent }}>{card.backInfo.type}</div>
                        </div>
                    </div>

                    {/* Bank info */}
                    <div className="px-4 mt-3 flex items-center justify-between">
                        <div>
                            <div className="text-[7px] text-white/20">{card.backInfo.bank}</div>
                            <div className="text-[7px] text-white/20">Customer Care: 1800-XXX-XXXX</div>
                        </div>
                        <NetworkBadge network={card.network} accent={card.accent} />
                    </div>
                </div>
            </div>
        </div>
    );
};

const CardsSection = () => {
    return (
        <section
            className="w-full py-20 px-6 relative overflow-hidden"
            style={{ background: "hsl(0 0% 6%)" }}
        >

            <div className="max-w-6xl mx-auto">
                {/* Header */}
                <div className="text-center mb-14">
                    <h2 className="text-4xl md:text-5xl font-black font-display text-foreground mb-4">
                        A card for <span className="text-primary">every need</span>
                    </h2>
                    <p className="text-muted-foreground text-base max-w-xl mx-auto">
                        From students to businesses — pick a card that fits your life.
                        <span className="block text-sm mt-1" style={{ color: "hsl(43 100% 55%)" }}>Hover on card to see back side ✦</span>
                    </p>
                </div>

                {/* Cards grid */}
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
                    {cards.map((card) => (
                        <div
                            key={card.id}
                            className="group rounded-3xl overflow-hidden transition-all duration-300 hover:-translate-y-1"
                            style={{
                                background: "hsl(0 0% 8%)",
                                border: "1px solid hsla(0,0%,100%,0.06)",
                                boxShadow: "0 4px 24px hsla(0,0%,0%,0.3)",
                            }}
                        >
                            {/* Flip card */}
                            <div className="p-5 pb-3">
                                <FlipCard card={card} />
                            </div>

                            {/* Info */}
                            <div className="px-5 pb-5">
                                {/* Badges */}
                                <div className="flex items-center gap-2 mb-3">
                                    <span className="px-2.5 py-0.5 rounded-full text-[10px] font-bold"
                                        style={{ background: "hsl(43 100% 55%)", color: "black" }}>
                                        {card.badge}
                                    </span>
                                    <span className="px-2.5 py-0.5 rounded-full text-[10px] font-medium text-muted-foreground"
                                        style={{ background: "hsla(0,0%,100%,0.05)", border: "1px solid hsla(0,0%,100%,0.08)" }}>
                                        {card.type}
                                    </span>
                                </div>

                                {/* Features */}
                                <ul className="space-y-2 mb-5">
                                    {card.features.map((f, i) => (
                                        <li key={i} className="flex items-center gap-2.5">
                                            <div className="w-4 h-4 rounded-full flex items-center justify-center shrink-0"
                                                style={{ background: "hsla(43,100%,55%,0.12)", border: "1px solid hsla(43,100%,55%,0.3)" }}>
                                                <svg width="8" height="6" viewBox="0 0 8 6" fill="none">
                                                    <path d="M1 3L2.8 4.8L7 1" stroke="hsl(43 100% 55%)" strokeWidth="1.2" strokeLinecap="round" strokeLinejoin="round" />
                                                </svg>
                                            </div>
                                            <span className="text-xs text-muted-foreground">{f}</span>
                                        </li>
                                    ))}
                                </ul>

                                {/* CTA — always gold */}
                                <button
                                    className="w-full py-2.5 rounded-xl text-xs font-bold flex items-center justify-center gap-2 transition-all duration-200 hover:opacity-90 active:scale-95"
                                    style={{
                                        background: "hsl(43 100% 55%)",
                                        color: "black",
                                        boxShadow: "0 4px 20px hsla(43,100%,55%,0.3)",
                                    }}
                                >
                                    Apply for {card.name}
                                    <svg width="12" height="12" viewBox="0 0 12 12" fill="none">
                                        <path d="M2 6H10M7 3L10 6L7 9" stroke="currentColor" strokeWidth="1.3" strokeLinecap="round" strokeLinejoin="round" />
                                    </svg>
                                </button>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </section>
    );
};

export default CardsSection;