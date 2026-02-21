import { ArrowRight, Shield, Zap, TrendingUp } from "lucide-react";
import CreditCard from "./CreditCard";


const HeroSection = () => {
    return (
        <section className="relative min-h-screen flex items-center overflow-hidden bg-radial-glow">
            <div
                className="absolute inset-0 opacity-[0.03]"
                style={{
                    backgroundImage: `linear-gradient(hsla(0,0%,100%,0.3) 1px, transparent 1px), linear-gradient(90deg, hsla(0,0%,100%,0.3) 1px, transparent 1px)`,
                    backgroundSize: "60px 60px",
                }}
            />

            <div className="absolute top-1/4 left-1/4 w-72 h-72 rounded-full bg-primary/5 blur-[100px] animate-pulse-slow" />
            <div className="absolute bottom-1/3 right-1/4 w-96 h-96 rounded-full bg-primary/3 blur-[120px] animate-pulse-slow" style={{ animationDelay: "2s" }} />

            <div className="relative z-10 max-w-7xl mx-auto px-6 pt-28 pb-16 w-full">
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-16 items-center">
                    <div>
                        <div className="animate-slide-up inline-flex items-center gap-2 px-4 py-1.5 rounded-full glass-card mb-8">
                            <div className="w-1.5 h-1.5 rounded-full bg-primary animate-pulse" />
                            <span className="text-xs font-medium text-muted-foreground">Trusted by 10,000+ customers worldwide</span>
                        </div>

                        <h1
                            className="animate-slide-up font-display text-4xl sm:text-5xl lg:text-6xl xl:text-7xl font-bold tracking-tight leading-[1.1] mb-6"
                            style={{ animationDelay: "0.1s" }}
                        >
                            The future of
                            <br />
                            <span className="text-gradient-gold">financial infrastructure</span>
                        </h1>
                        <div
                            className="animate-slide-up flex flex-col sm:flex-row items-start gap-4 mb-12"
                            style={{ animationDelay: "0.3s" }}
                        >
                            <button className="w-full sm:w-auto bg-primary cursor-pointer text-primary-foreground px-8 py-3.5 rounded-xl text-sm font-semibold flex items-center justify-center gap-2 hover:opacity-90 transition-all glow-gold">
                                Open Account <ArrowRight className="w-4 h-4" />
                            </button>
                            <button className="w-full sm:w-auto glass-card px-8 py-3.5 cursor-pointer rounded-xl text-sm font-medium text-foreground hover:bg-secondary transition-all">
                                Link instantly
                            </button>
                        </div>
                    </div>
                    <div
                        className="animate-fade-in flex items-center justify-center lg:justify-end"
                        style={{ animationDelay: "0.5s" }}
                    >
                        <CreditCard />
                    </div>
                </div>
            </div>
        </section>
    );
};

export default HeroSection;
