import { useEffect, useRef } from "react";
import Rbi from "../assets/rbi.png";

const banks = [
    { name: "PNB", logo: "/src/assets/punjab.png" },
    { name: "ICICI", logo: "/src/assets/icic.png" },
    { name: "SBI", logo: "/src/assets/sbi.png" },
    { name: "Axis", logo: "/src/assets/axis.png" },
    { name: "Kotak", logo: "/src/assets/bank.png" },
    { name: "BoB", logo: "/src/assets/bob.png" },
    { name: "Federal", logo: "/src/assets/new.png" },
    { name: "Canara", logo: "/src/assets/hdfc.png" },
    { name: "HDFC", logo: "/src/assets/hdfc.png" },
];

const RADIUS = 140;

const BankLinkSection = () => {
    const iconsRef = useRef([]);
    const angleRef = useRef(0);
    const rafRef = useRef(null);
    const startTimeRef = useRef(null);

    useEffect(() => {
        const animate = (time) => {
            if (!startTimeRef.current) startTimeRef.current = time;
            const elapsed = time - startTimeRef.current;

            angleRef.current += 0.4;
            const a = angleRef.current;

            iconsRef.current.forEach((el, i) => {
                if (!el) return;

                const base = (i / banks.length) * 2 * Math.PI;
                const rad = base + (a * Math.PI) / 180;

                // stagger animation
                const delay = i * 120;
                const progress = Math.min(
                    1,
                    Math.max(0, (elapsed - delay) / 800)
                );

                const x = RADIUS * progress * Math.cos(rad);
                const y = RADIUS * progress * Math.sin(rad);

                el.style.transform = `
          translate(${x}px, ${y}px)
          rotate(${-a}deg)
          scale(${progress})
        `;
                el.style.opacity = progress;
            });

            rafRef.current = requestAnimationFrame(animate);
        };

        rafRef.current = requestAnimationFrame(animate);
        return () => cancelAnimationFrame(rafRef.current);
    }, []);

    return (
        <section className="w-full -mt-50 py-24 px-6 relative overflow-hidden flex flex-col items-center"
            style={{ background: "hsl(0 0% 6%)" }}>
            <div className="absolute w-[500px] h-[500px] rounded-full bg-yellow-600/10 blur-3xl" />
            <div
                className="relative flex items-center justify-center mb-16"
                style={{ width: 360, height: 360 }}
            >

                {banks.map((bank, i) => (
                    <div
                        key={bank.name}
                        ref={(el) => (iconsRef.current[i] = el)}
                        className="absolute flex items-center justify-center rounded-2xl shadow-lg select-none"
                        style={{
                            width: 70,
                            height: 70,
                            opacity: 0,
                            transform: "translate(0px,0px) scale(0)",
                            willChange: "transform",
                        }}
                    >
                        <img
                            src={bank.logo}
                            alt={bank.name}
                            className="w-20 object-contain"
                            draggable={false}
                        />
                    </div>
                ))}

                <div className="relative z-10 flex items-center justify-center">
                    <div className="absolute w-40 h-40 rounded-full bg-yellow-500/20 blur-2xl animate-pulse" />
                    <img
                        src={Rbi}
                        alt="RBI Logo"
                        className="w-32 object-contain relative z-10"
                        draggable={false}
                    />
                </div>
            </div>

            <div className="text-center max-w-3xl px-4">
                <h2 className="text-4xl md:text-5xl font-black text-white leading-tight">
                    Link your bank account
                </h2>
                <p className="text-3xl md:text-5xl font-bold mt-3 text-gray-500">
                    and make limitless payments
                </p>
            </div>
            <div className="w-full py-10 px-6 relative overflow-hidden flex flex-col items-center "
                style={{ background: "hsl(0 0% 6%)" }}>
                <div className="flex items-center gap-3 mb-6">
                    <div className="flex -space-x-3">
                        <div className="w-8 h-8 rounded-full bg-gray-400 border border-black" />
                        <div className="w-8 h-8 rounded-full bg-gray-500 border border-black" />
                        <div className="w-8 h-8 rounded-full bg-gray-600 border border-black" />
                    </div>
                    <p className="text-sm text-gray-300 font-medium">
                        200K+ users pay using Bank
                    </p>
                </div>

                <button className="px-10 py-2 cursor-pointer rounded-full bg-white text-black font-semibold text-lg hover:scale-105 transition duration-300 mb-16">
                    Link instantly
                </button>

                <div className="text-center max-w-4xl px-4">
                    <h2 className="text-2xl md:text-3xl font-black text-white leading-tight">
                        What we're launching with UPI 2.0
                    </h2>
                </div>

            </div>
        </section>
    );
};

export default BankLinkSection;