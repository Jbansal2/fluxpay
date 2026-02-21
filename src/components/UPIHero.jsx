import React from "react";

const UPIHero = () => {
    return (
        <section className="relative w-full min-h-screen flex  justify-center overflow-hidden px-6"
            style={{ background: "hsl(0 0% 6%)" }}>

            <div
                className="absolute inset-0 opacity-[0.08] pointer-events-none"
                style={{
                    backgroundImage:
                        "radial-gradient(circle, rgba(255,255,255,0.2) 1px, transparent 1px)",
                    backgroundSize: "24px 24px",
                }}
            />

            <div className="relative z-10 text-center max-w-5xl mt-20">

                <p className="text-yellow-400 text-sm md:text-lg font-semibold mb-4 tracking-wide">
                    introducing
                </p>

                <h1 className="text-5xl sm:text-7xl md:text-7xl font-extrabold mb-8 bg-gradient-to-b from-yellow-300 to-yellow-500 bg-clip-text text-transparent drop-shadow-[0_4px_20px_rgba(234,179,8,0.4)]">
                    UPI 2.0
                </h1>

                <h2 className="text-5xl sm:text-6xl md:text-7xl lg:text-7xl font-bold text-white leading-tight">
                    Now you never
                    <br className="hidden sm:block" />
                    have to leave us.
                </h2>

            </div>
        </section>
    );
};

export default UPIHero;