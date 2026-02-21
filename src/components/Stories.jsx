import { useState, useEffect, useRef } from "react";

const stories = [
    {
        name: "Aditya",
        title: "Love the concept of spending account",
        review: "The idea is so cool. I can use my spending account to make daily payments - and keep it separate from my savings now!",
        initials: "A",
        avatarBg: "linear-gradient(135deg, #f97316, #ef4444)",
        cardBg: "linear-gradient(135deg, #fff1e6, #ffe0d0)",
        accent: "#f97316",
    },
    {
        name: "Priya",
        title: "Best finance app for teens",
        review: "Finally an app that understands what young people need. The UPI payments and instant transfers are super smooth!",
        initials: "P",
        avatarBg: "linear-gradient(135deg, #8b5cf6, #6366f1)",
        cardBg: "linear-gradient(135deg, #ede9fe, #e0e7ff)",
        accent: "#8b5cf6",
    },
    {
        name: "Rahul",
        title: "My savings have never been better",
        review: "The investment features are amazing. I started saving small amounts and now I can see real growth every month!",
        initials: "R",
        avatarBg: "linear-gradient(135deg, #10b981, #059669)",
        cardBg: "linear-gradient(135deg, #d1fae5, #ccfbf1)",
        accent: "#10b981",
    },
    {
        name: "Sneha",
        title: "Love the cashback rewards",
        review: "Every time I spend I get cashback. It feels like the app is rewarding me for just living my life. Absolutely love it!",
        initials: "S",
        avatarBg: "linear-gradient(135deg, #ec4899, #f59e0b)",
        cardBg: "linear-gradient(135deg, #fce7f3, #fef9c3)",
        accent: "#ec4899",
    },
];

const CARD_W = 220;
const CARD_H = 300;

const Stories = () => {
    const [active, setActive] = useState(0);
    const [animDir, setAnimDir] = useState(null); // 'left' | 'right'
    const [isAnimating, setIsAnimating] = useState(false);
    const timerRef = useRef(null);

    const startTimer = () => {
        clearInterval(timerRef.current);
        timerRef.current = setInterval(() => goTo("right"), 3800);
    };

    useEffect(() => {
        startTimer();
        return () => clearInterval(timerRef.current);
    }, [active]);

    const goTo = (dir) => {
        if (isAnimating) return;
        setIsAnimating(true);
        setAnimDir(dir);
        setTimeout(() => {
            setActive((prev) =>
                dir === "right"
                    ? (prev + 1) % stories.length
                    : (prev - 1 + stories.length) % stories.length
            );
            setIsAnimating(false);
            setAnimDir(null);
        }, 400);
    };

    const handleDot = (i) => {
        if (i === active || isAnimating) return;
        goTo(i > active ? "right" : "left");
        startTimer();
    };

    const prev = (active - 1 + stories.length) % stories.length;
    const next = (active + 1) % stories.length;

    // Card positions: back, mid, front
    const getCardStyle = (index, position) => {
        const base = {
            position: "absolute",
            width: CARD_W,
            height: CARD_H,
            borderRadius: 20,
            overflow: "hidden",
            background: stories[index].cardBg,
            transition: "all 0.4s cubic-bezier(0.4, 0, 0.2, 1)",
        };

        if (position === "back") return { ...base, top: 24, left: 0, opacity: 0.35, transform: "rotate(-7deg) scale(0.92)", zIndex: 1 };
        if (position === "mid") return { ...base, top: 12, left: 20, opacity: 0.65, transform: "rotate(-3deg) scale(0.96)", zIndex: 2 };
        if (position === "front") return { ...base, top: 0, left: 40, opacity: 1, transform: "rotate(0deg) scale(1)", zIndex: 3 };
    };

    return (
        <section className="w-full py-25 px-6" style={{ background: "hsl(0 0% 6%)" }}>
            {/* Heading */}
            <h2 className="text-center text-5xl md:text-5xl font-bold text-foreground mb-20 font-display">
                Over a <span className="text-primary">million</span> happy stories
            </h2>

            <div className="max-w-5xl mt-10 mx-auto flex flex-col md:flex-row items-center gap-20">

                {/* Card Stack */}
                <div className="relative shrink-0" style={{ width: 300, height: 340 }}>

                    {/* Back */}
                    <div style={getCardStyle(prev, "back")}>
                        <CardInner story={stories[prev]} />
                    </div>

                    {/* Mid */}
                    <div style={getCardStyle(next, "mid")}>
                        <CardInner story={stories[next]} />
                    </div>

                    {/* Front — animated */}
                    <div
                        style={{
                            ...getCardStyle(active, "front"),
                            animation: animDir
                                ? `${animDir === "right" ? "swipeLeft" : "swipeRight"} 0.4s cubic-bezier(0.4,0,0.2,1) forwards`
                                : "none",
                        }}
                    >
                        <CardInner story={stories[active]} />
                    </div>

                    {/* Arrow buttons */}
                    <button
                        onClick={() => { goTo("left"); startTimer(); }}
                        className="absolute -left-4 top-1/2 -translate-y-1/2 w-8 h-8 rounded-full flex items-center justify-center z-10 cursor-pointer"
                        style={{ background: "hsl(0 0% 12%)", border: "1px solid hsl(0 0% 20%)" }}
                    >
                        <span className="text-muted-foreground text-sm">‹</span>
                    </button>
                    <button
                        onClick={() => { goTo("right"); startTimer(); }}
                        className="absolute -right-0 top-1/2 -translate-y-1/2 w-8 h-8 rounded-full flex items-center justify-center z-10 cursor-pointer"
                        style={{ background: "hsl(0 0% 12%)", border: "1px solid hsl(0 0% 20%)" }}
                    >
                        <span className="text-muted-foreground text-sm">›</span>
                    </button>
                </div>

                {/* Text */}
                <div
                    className="flex-1 max-w-md"
                    key={active}
                    style={{ animation: "fadeUp 0.45s ease forwards" }}
                >
                    {/* Quote mark */}
                    <div className="text-5xl font-display mb-2" style={{ color: stories[active].accent, lineHeight: 1 }}>"</div>

                    <h3 className="text-xl md:text-2xl font-bold text-foreground mb-4 font-display leading-snug">
                        {stories[active].title}
                    </h3>
                    <p className="text-sm text-muted-foreground leading-relaxed mb-6">
                        {stories[active].review}
                    </p>

                    {/* Author row */}
                    <div className="flex items-center gap-3">
                        <div
                            className="w-9 h-9 rounded-full flex items-center justify-center text-white text-sm font-bold shrink-0"
                            style={{ background: stories[active].avatarBg }}
                        >
                            {stories[active].initials}
                        </div>
                        <div>
                            <div className="text-sm font-semibold text-foreground">{stories[active].name}</div>
                            <div className="text-[11px] text-muted-foreground">Verified User</div>
                        </div>
                        {/* Stars */}
                        <div className="ml-auto flex gap-0.5">
                            {Array.from({ length: 5 }).map((_, i) => (
                                <span key={i} className="text-xs" style={{ color: stories[active].accent }}>★</span>
                            ))}
                        </div>
                    </div>

                    {/* Dots */}
                    <div className="flex gap-2 mt-8">
                        {stories.map((_, i) => (
                            <button
                                key={i}
                                onClick={() => handleDot(i)}
                                className="h-1.5 rounded-full cursor-pointer"
                                style={{
                                    width: i === active ? 28 : 8,
                                    background: i === active ? stories[active].accent : "hsl(0 0% 22%)",
                                    transition: "all 0.3s ease",
                                }}
                            />
                        ))}
                    </div>
                </div>
            </div>

            <style>{`
        @keyframes fadeUp {
          from { opacity: 0; transform: translateY(18px); }
          to   { opacity: 1; transform: translateY(0); }
        }
        @keyframes swipeLeft {
          from { opacity: 1; transform: rotate(0deg) scale(1) translateX(0); }
          to   { opacity: 0; transform: rotate(-4deg) scale(0.94) translateX(-60px); }
        }
        @keyframes swipeRight {
          from { opacity: 1; transform: rotate(0deg) scale(1) translateX(0); }
          to   { opacity: 0; transform: rotate(4deg) scale(0.94) translateX(60px); }
        }
      `}</style>
        </section>
    );
};

const CardInner = ({ story }) => (
    <div className="w-full h-full relative flex flex-col items-center justify-center gap-3 p-5">
        {/* Decorative circles */}
        <div className="absolute -top-8 -right-8 w-32 h-32 rounded-full opacity-20"
            style={{ background: story.accent }} />
        <div className="absolute -bottom-10 -left-10 w-36 h-36 rounded-full opacity-10"
            style={{ background: story.accent }} />

        {/* Avatar */}
        <div
            className="w-20 h-20 rounded-full flex items-center justify-center text-white text-3xl font-bold shadow-lg relative z-10"
            style={{ background: story.avatarBg }}
        >
            {story.initials}
        </div>

        {/* Name tag */}
        <div
            className="relative z-10 px-4 py-1 rounded-full text-xs font-semibold text-white"
            style={{ background: "hsla(0,0%,0%,0.25)", backdropFilter: "blur(8px)" }}
        >
            {story.name}
        </div>

        {/* Mini stars */}
        <div className="flex gap-0.5 relative z-10">
            {Array.from({ length: 5 }).map((_, i) => (
                <span key={i} className="text-[10px]" style={{ color: story.accent }}>★</span>
            ))}
        </div>
    </div>
);

export default Stories;