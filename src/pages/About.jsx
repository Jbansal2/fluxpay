import { useEffect, useRef, useState } from "react";

const useInView = (threshold = 0.15) => {
  const ref = useRef(null);
  const [inView, setInView] = useState(false);
  useEffect(() => {
    const obs = new IntersectionObserver(([e]) => { if (e.isIntersecting) setInView(true); }, { threshold });
    if (ref.current) obs.observe(ref.current);
    return () => obs.disconnect();
  }, []);
  return [ref, inView];
};

const Counter = ({ target, suffix = "", duration = 2000 }) => {
  const [count, setCount] = useState(0);
  const [ref, inView] = useInView(0.5);
  useEffect(() => {
    if (!inView) return;
    let start = 0;
    const step = target / (duration / 16);
    const timer = setInterval(() => {
      start += step;
      if (start >= target) { setCount(target); clearInterval(timer); }
      else setCount(Math.floor(start));
    }, 16);
    return () => clearInterval(timer);
  }, [inView]);
  return <span ref={ref}>{count.toLocaleString()}{suffix}</span>;
};

const Reveal = ({ children, delay = 0, direction = "up", className = "" }) => {
  const [ref, inView] = useInView();
  const transforms = { up: "translateY(40px)", down: "translateY(-40px)", left: "translateX(-40px)", right: "translateX(40px)", none: "scale(0.95)" };
  return (
    <div ref={ref} className={className} style={{
      opacity: inView ? 1 : 0,
      transform: inView ? "none" : transforms[direction],
      transition: `opacity 0.8s ease ${delay}s, transform 0.8s cubic-bezier(0.22,1,0.36,1) ${delay}s`,
    }}>
      {children}
    </div>
  );
};

const Orb = ({ size, top, left, color, delay = 0, blur = 80 }) => (
  <div className="absolute rounded-full pointer-events-none"
    style={{
      width: size, height: size, top, left,
      background: color, filter: `blur(${blur}px)`, opacity: 0.4,
      animation: `float ${6 + delay}s ease-in-out infinite`,
      animationDelay: `${delay}s`,
    }} />
);

const ScrambleText = ({ text, trigger }) => {
  const [display, setDisplay] = useState(text);
  const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#$%";
  useEffect(() => {
    if (!trigger) return;
    let iterations = 0;
    const max = text.length * 3;
    const interval = setInterval(() => {
      setDisplay(text.split("").map((char, i) => {
        if (char === " ") return " ";
        if (i < iterations / 3) return char;
        return chars[Math.floor(Math.random() * chars.length)];
      }).join(""));
      iterations++;
      if (iterations >= max) { setDisplay(text); clearInterval(interval); }
    }, 30);
    return () => clearInterval(interval);
  }, [trigger]);
  return <span>{display}</span>;
};

const TeamCard = ({ name, role, initials, gradient, delay }) => {
  const [hovered, setHovered] = useState(false);
  const [ref, inView] = useInView();
  return (
    <div ref={ref} style={{
      opacity: inView ? 1 : 0,
      transform: inView ? "none" : "translateY(50px)",
      transition: `opacity 0.7s ease ${delay}s, transform 0.7s cubic-bezier(0.22,1,0.36,1) ${delay}s`,
    }}>
      <div
        className="relative rounded-3xl p-6 cursor-pointer overflow-hidden"
        style={{
          background: "hsl(0 0% 8%)",
          border: hovered ? "1px solid hsla(43,100%,55%,0.3)" : "1px solid hsla(0,0%,100%,0.06)",
          transform: hovered ? "translateY(-8px)" : "translateY(0)",
          transition: "all 0.4s cubic-bezier(0.22,1,0.36,1)",
          boxShadow: hovered ? "0 20px 60px hsla(43,100%,55%,0.1)" : "0 4px 20px hsla(0,0%,0%,0.3)",
        }}
        onMouseEnter={() => setHovered(true)}
        onMouseLeave={() => setHovered(false)}
      >
        {/* Gold glow on hover */}
        <div className="absolute inset-0 rounded-3xl pointer-events-none" style={{
          background: hovered ? "radial-gradient(circle at 50% 0%, hsla(43,100%,55%,0.06), transparent 70%)" : "none",
          transition: "background 0.4s ease",
        }} />

        {/* Avatar */}
        <div className="w-16 h-16 rounded-2xl flex items-center justify-center text-xl font-black text-black mb-4 relative"
          style={{ background: gradient }}>
          <span>{initials}</span>
          {hovered && (
            <div className="absolute inset-0 rounded-2xl animate-ping opacity-20" style={{ background: gradient }} />
          )}
        </div>

        <div className="text-sm font-bold text-white mb-1">{name}</div>
        <div className="text-xs" style={{ color: "hsl(43 100% 55%)" }}>{role}</div>

        {/* Decorative line */}
        <div className="mt-4 h-px w-full" style={{
          background: hovered
            ? `linear-gradient(90deg, ${gradient.includes("43") ? "hsl(43 100% 55%)" : "hsl(260 80% 70%)"}, transparent)`
            : "hsla(0,0%,100%,0.06)",
          transition: "background 0.4s ease",
        }} />
      </div>
    </div>
  );
};

const TimelineItem = ({ year, title, desc, delay, isLast }) => {
  const [ref, inView] = useInView();
  return (
    <div ref={ref} className="flex gap-6" style={{
      opacity: inView ? 1 : 0,
      transform: inView ? "none" : "translateX(-30px)",
      transition: `opacity 0.7s ease ${delay}s, transform 0.7s cubic-bezier(0.22,1,0.36,1) ${delay}s`,
    }}>
      {/* Left: year + line */}
      <div className="flex flex-col items-center">
        <div className="w-12 h-12 rounded-xl flex items-center justify-center text-xs font-black text-black shrink-0"
          style={{ background: "hsl(43 100% 55%)" }}>{year}</div>
        {!isLast && <div className="w-px flex-1 mt-3" style={{ background: "linear-gradient(180deg, hsla(43,100%,55%,0.4), transparent)" }} />}
      </div>
      {/* Right: content */}
      <div className="pb-10">
        <div className="text-base font-bold text-white mb-1">{title}</div>
        <div className="text-sm text-muted-foreground leading-relaxed">{desc}</div>
      </div>
    </div>
  );
};

const ValueCard = ({ icon, title, desc, delay }) => {
  const [hovered, setHovered] = useState(false);
  const [ref, inView] = useInView();
  return (
    <div ref={ref} style={{
      opacity: inView ? 1 : 0,
      transform: inView ? "none" : "translateY(40px)",
      transition: `opacity 0.7s ease ${delay}s, transform 0.7s cubic-bezier(0.22,1,0.36,1) ${delay}s`,
    }}>
      <div
        className="rounded-3xl p-6 h-full relative overflow-hidden"
        style={{
          background: "hsl(0 0% 8%)",
          border: hovered ? "1px solid hsla(43,100%,55%,0.25)" : "1px solid hsla(0,0%,100%,0.06)",
          transform: hovered ? "scale(1.02)" : "scale(1)",
          transition: "all 0.3s ease",
        }}
        onMouseEnter={() => setHovered(true)}
        onMouseLeave={() => setHovered(false)}
      >
        <div className="absolute inset-0 opacity-[0.03] rounded-3xl" style={{
          backgroundImage: `linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)`,
          backgroundSize: "28px 28px",
        }} />
        <div className="text-3xl mb-4">{icon}</div>
        <div className="text-base font-bold text-white mb-2">{title}</div>
        <div className="text-sm text-muted-foreground leading-relaxed">{desc}</div>
      </div>
    </div>
  );
};

const About = () => {
  const heroRef = useRef(null);
  const [heroVisible, setHeroVisible] = useState(false);
  const [mousePos, setMousePos] = useState({ x: 0, y: 0 });
  const [missionRef, missionInView] = useInView(0.3);

  useEffect(() => {
    const timer = setTimeout(() => setHeroVisible(true), 100);
    return () => clearTimeout(timer);
  }, []);

  useEffect(() => {
    const move = (e) => setMousePos({ x: e.clientX, y: e.clientY });
    window.addEventListener("mousemove", move);
    return () => window.removeEventListener("mousemove", move);
  }, []);

  const stats = [
    { value: 10, suffix: "M+", label: "Active Users" },
    { value: 500, suffix: "Cr+", label: "Transactions Daily" },
    { value: 99, suffix: ".9%", label: "Uptime SLA" },
    { value: 50, suffix: "+", label: "Team Members" },
  ];

  const team = [
    { name: "Arjun Mehta", role: "CEO & Co-founder", initials: "AM", gradient: "hsl(43 100% 55%)", delay: 0.1 },
    { name: "Riya Sharma", role: "CTO & Co-founder", initials: "RS", gradient: "linear-gradient(135deg, hsl(260 80% 70%), hsl(200 100% 60%))", delay: 0.2 },
    { name: "Karan Patel", role: "Head of Design", initials: "KP", gradient: "linear-gradient(135deg, hsl(0 80% 60%), hsl(30 100% 60%))", delay: 0.3 },
    { name: "Priya Nair", role: "Head of Product", initials: "PN", gradient: "linear-gradient(135deg, hsl(120 60% 55%), hsl(200 100% 60%))", delay: 0.4 },
    { name: "Vikram Singh", role: "VP Engineering", initials: "VS", gradient: "linear-gradient(135deg, hsl(43 100% 55%), hsl(120 60% 55%))", delay: 0.5 },
    { name: "Sneha Roy", role: "Head of Growth", initials: "SR", gradient: "linear-gradient(135deg, hsl(260 80% 70%), hsl(0 80% 60%))", delay: 0.6 },
  ];

  const timeline = [
    { year: "2020", title: "The Idea", desc: "Two college friends frustrated with complex banking apps decide to build something different — a financial app for India's youth.", delay: 0.1 },
    { year: "2021", title: "First Launch", desc: "FluxPay launches in beta with 1,000 users. The UPI integration and custom QR themes go viral on Instagram.", delay: 0.2 },
    { year: "2022", title: "Series A", desc: "Raised ₹50 Cr in Series A. Launched FluxX Teens — India's first financial product built specifically for teenagers.", delay: 0.3 },
    { year: "2023", title: "10M Users", desc: "Hit 10 million users milestone. Launched FluxPay Card — India's first numberless prepaid card with doodle customization.", delay: 0.4 },
    { year: "2024", title: "UPI 2.0", desc: "Launched UPI 2.0 — bank account linking that makes FluxPay a complete financial OS. RBI regulated & NPCI certified.", delay: 0.5 },
    { year: "2025", title: "What's Next", desc: "International expansion, credit products, and AI-powered financial guidance. The future of money is here.", delay: 0.6, isLast: true },
  ];

  const values = [
    { icon: "⚡", title: "Speed First", desc: "Every product decision optimizes for speed. Money should move as fast as thought.", delay: 0.1 },
    { icon: "🔐", title: "Privacy by Design", desc: "Your financial data belongs to you. We build security in from day one, not as an afterthought.", delay: 0.2 },
    { icon: "🎨", title: "Design Obsessed", desc: "Finance doesn't have to be boring. We believe great design is a feature, not a decoration.", delay: 0.3 },
    { icon: "🇮🇳", title: "Built for India", desc: "Designed for the way India transacts — UPI, RuPay, regional languages, and Bharat-first features.", delay: 0.4 },
    { icon: "🚀", title: "Move Fast", desc: "We ship daily. Learn weekly. Iterate always. Bureaucracy is the enemy of great products.", delay: 0.5 },
    { icon: "💛", title: "User Love", desc: "Every feature is driven by a real user need. We read every review, reply to every tweet, ship every fix.", delay: 0.6 },
  ];

  return (
    <div className="min-h-screen overflow-x-hidden" style={{ background: "hsl(0 0% 4%)" }}>

      <style>{`
        @keyframes float {
          0%, 100% { transform: translateY(0px) rotate(0deg); }
          33% { transform: translateY(-20px) rotate(2deg); }
          66% { transform: translateY(-10px) rotate(-1deg); }
        }
        @keyframes slideDown {
          from { opacity: 0; transform: translateY(-30px); }
          to   { opacity: 1; transform: translateY(0); }
        }
        @keyframes slideUp {
          from { opacity: 0; transform: translateY(60px); }
          to   { opacity: 1; transform: translateY(0); }
        }
        @keyframes fadeIn {
          from { opacity: 0; }
          to   { opacity: 1; }
        }
        @keyframes ticker {
          0%   { transform: translateX(0); }
          100% { transform: translateX(-50%); }
        }
        @keyframes pulse-ring {
          0%   { transform: scale(1); opacity: 0.6; }
          100% { transform: scale(1.8); opacity: 0; }
        }
        @keyframes spin-slow {
          from { transform: rotate(0deg); }
          to   { transform: rotate(360deg); }
        }
        @keyframes gradient-shift {
          0%, 100% { background-position: 0% 50%; }
          50%       { background-position: 100% 50%; }
        }
        .gradient-text {
          background: linear-gradient(135deg, hsl(43 100% 80%), hsl(43 100% 55%), hsl(35 100% 45%));
          background-size: 200% 200%;
          animation: gradient-shift 4s ease infinite;
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
        }
      `}</style>

      <section className="relative min-h-screen flex flex-col items-center justify-center px-6 overflow-hidden">

        <Orb size="500px" top="-10%" left="-10%" color="hsla(43,100%,55%,0.15)" delay={0} blur={120} />
        <Orb size="400px" top="20%" left="70%" color="hsla(260,80%,60%,0.1)" delay={2} blur={100} />
        <Orb size="300px" top="60%" left="10%" color="hsla(120,60%,50%,0.08)" delay={4} blur={90} />

        <div className="absolute pointer-events-none rounded-full" style={{
          width: 600, height: 600,
          left: mousePos.x - 300, top: mousePos.y - 300,
          background: "radial-gradient(circle, hsla(43,100%,55%,0.05), transparent 60%)",
          transition: "left 0.3s ease, top 0.3s ease",
          zIndex: 0,
        }} />

        <div className="absolute inset-0 pointer-events-none" style={{
          backgroundImage: "radial-gradient(circle, rgba(255,255,255,0.08) 1px, transparent 1px)",
          backgroundSize: "32px 32px",
          opacity: 0.5,
        }} />

        <div className="absolute top-20 right-20 opacity-10 hidden md:block" style={{ animation: "spin-slow 20s linear infinite" }}>
          <svg width="200" height="200" viewBox="0 0 200 200" fill="none">
            <circle cx="100" cy="100" r="90" stroke="hsl(43 100% 55%)" strokeWidth="1" strokeDasharray="8 8" />
            <circle cx="100" cy="100" r="70" stroke="hsl(43 100% 55%)" strokeWidth="0.5" strokeDasharray="4 12" />
          </svg>
        </div>
        <div className="absolute bottom-20 left-20 opacity-10 hidden md:block" style={{ animation: "spin-slow 15s linear infinite reverse" }}>
          <svg width="140" height="140" viewBox="0 0 140 140" fill="none">
            <circle cx="70" cy="70" r="60" stroke="hsl(260 80% 70%)" strokeWidth="1" strokeDasharray="6 6" />
          </svg>
        </div>

        <div className="relative z-10 text-center max-w-4xl mx-auto">
          <div style={{
            opacity: heroVisible ? 1 : 0,
            transform: heroVisible ? "none" : "translateY(-20px)",
            transition: "opacity 0.6s ease 0.1s, transform 0.6s ease 0.1s",
          }}>
            <div className="inline-flex items-center gap-2 px-4 py-2 rounded-full text-xs font-semibold mb-8"
              style={{ background: "hsla(43,100%,55%,0.1)", border: "1px solid hsla(43,100%,55%,0.25)", color: "hsl(43 100% 55%)" }}>
              <span className="w-1.5 h-1.5 rounded-full bg-current animate-pulse" />
              Our Story — Founded 2020
            </div>
          </div>

          <div style={{
            opacity: heroVisible ? 1 : 0,
            transform: heroVisible ? "none" : "translateY(40px)",
            transition: "opacity 0.8s ease 0.2s, transform 0.8s cubic-bezier(0.22,1,0.36,1) 0.2s",
          }}>
            <h1 className="text-5xl md:text-7xl lg:text-8xl font-black leading-[0.95] mb-8 tracking-tight">
              <span className="text-white block">We're building</span>
              <span className="gradient-text block">the future of</span>
              <span className="text-white block">money in India</span>
            </h1>
          </div>

          <div style={{
            opacity: heroVisible ? 1 : 0,
            transform: heroVisible ? "none" : "translateY(30px)",
            transition: "opacity 0.8s ease 0.4s, transform 0.8s ease 0.4s",
          }}>
            <p className="text-lg md:text-xl text-muted-foreground max-w-2xl mx-auto leading-relaxed mb-10">
              FluxPay started with a simple belief — that financial tools should be as intuitive as texting a friend.
              We're a team of 50+ builders, designers, and dreamers reimagining how India interacts with money.
            </p>
          </div>

          <div className="flex flex-wrap items-center justify-center gap-4" style={{
            opacity: heroVisible ? 1 : 0,
            transition: "opacity 0.8s ease 0.6s",
          }}>
            <button className="px-8 py-3.5 rounded-full font-bold text-sm text-black transition-all hover:scale-105 active:scale-95"
              style={{ background: "hsl(43 100% 55%)", boxShadow: "0 8px 32px hsla(43,100%,55%,0.35)" }}>
              Join Our Team →
            </button>
            <button className="px-8 py-3.5 rounded-full font-bold text-sm text-white transition-all hover:scale-105"
              style={{ background: "hsla(0,0%,100%,0.06)", border: "1px solid hsla(0,0%,100%,0.1)" }}>
              Read Our Story
            </button>
          </div>
        </div>

        <div className="absolute bottom-8 left-1/2 -translate-x-1/2 flex flex-col items-center gap-2" style={{
          opacity: heroVisible ? 1 : 0,
          transition: "opacity 1s ease 1s",
        }}>
          <span className="text-xs text-muted-foreground tracking-widest">SCROLL</span>
          <div className="w-px h-10 relative overflow-hidden" style={{ background: "hsla(0,0%,100%,0.1)" }}>
            <div className="absolute w-full bg-primary" style={{
              height: "40%", top: 0,
              animation: "slideDown 1.5s ease-in-out infinite",
            }} />
          </div>
        </div>
      </section>

      <div className="relative py-5 overflow-hidden border-y" style={{ borderColor: "hsla(43,100%,55%,0.15)", background: "hsla(43,100%,55%,0.03)" }}>
        <div className="flex gap-0" style={{ animation: "ticker 20s linear infinite", width: "max-content" }}>
          {[...Array(2)].map((_, ri) => (
            <div key={ri} className="flex items-center gap-8 px-4">
              {["10M+ Users", "✦", "₹500Cr Daily Volume", "✦", "RBI Regulated", "✦", "NPCI Certified", "✦", "ISO 27001", "✦", "99.9% Uptime", "✦", "50+ Team Members", "✦", "Founded 2020", "✦"].map((item, i) => (
                <span key={i} className="text-sm font-semibold whitespace-nowrap"
                  style={{ color: item === "✦" ? "hsl(43 100% 55%)" : "hsl(0 0% 50%)" }}>
                  {item}
                </span>
              ))}
            </div>
          ))}
        </div>
      </div>

      <section className="py-24 px-6 relative">
        <div className="absolute inset-0 pointer-events-none" style={{
          background: "radial-gradient(ellipse at 50% 50%, hsla(43,100%,55%,0.04), transparent 70%)",
        }} />
        <div className="max-w-5xl mx-auto">
          <div className="grid grid-cols-2 md:grid-cols-4 gap-6">
            {stats.map((s, i) => (
              <Reveal key={i} delay={i * 0.1} direction="up">
                <div className="text-center p-8 rounded-3xl relative overflow-hidden group"
                  style={{ background: "hsl(0 0% 8%)", border: "1px solid hsla(0,0%,100%,0.06)" }}>
                  <div className="absolute inset-0 rounded-3xl opacity-0 group-hover:opacity-100 transition-opacity duration-500"
                    style={{ background: "radial-gradient(circle at 50% 0%, hsla(43,100%,55%,0.06), transparent 70%)" }} />
                  <div className="text-4xl md:text-5xl font-black mb-2 gradient-text">
                    <Counter target={s.value} suffix={s.suffix} />
                  </div>
                  <div className="text-sm text-muted-foreground font-medium">{s.label}</div>
                </div>
              </Reveal>
            ))}
          </div>
        </div>
      </section>

      <section className="py-24 px-6 relative overflow-hidden">
        <Orb size="600px" top="0%" left="50%" color="hsla(43,100%,55%,0.06)" delay={1} blur={150} />
        <div className="max-w-5xl mx-auto">
          <div className="grid md:grid-cols-2 gap-16 items-center">
            <div>
              <Reveal delay={0.1}>
                <div className="text-xs font-semibold tracking-widest mb-4" style={{ color: "hsl(43 100% 55%)" }}>OUR MISSION</div>
              </Reveal>
              <Reveal delay={0.2}>
                <h2 className="text-4xl md:text-5xl font-black text-white leading-tight mb-6">
                  Financial freedom for every Indian
                </h2>
              </Reveal>
              <Reveal delay={0.3}>
                <p className="text-muted-foreground leading-relaxed mb-6">
                  India has 1.4 billion people. Only a fraction have access to financial tools that actually work for their lifestyle.
                  We're changing that — one UPI payment, one savings goal, one teen account at a time.
                </p>
              </Reveal>
              <Reveal delay={0.4}>
                <p className="text-muted-foreground leading-relaxed">
                  From a student paying hostel rent to a business owner managing GST — FluxPay is being built for real India, not just tier-1 cities.
                </p>
              </Reveal>
            </div>

            <Reveal delay={0.2} direction="left">
              <div ref={missionRef} className="relative">
                <div className="rounded-3xl p-8 relative overflow-hidden"
                  style={{
                    background: "linear-gradient(135deg, hsl(0 0% 9%), hsl(30 15% 11%))",
                    border: "1px solid hsla(43,100%,55%,0.15)",
                    boxShadow: "0 40px 80px hsla(0,0%,0%,0.5)",
                  }}>
                  <div className="absolute inset-0 opacity-[0.04] rounded-3xl" style={{
                    backgroundImage: `linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)`,
                    backgroundSize: "28px 28px",
                  }} />
                  <div className="absolute -top-10 -right-10 w-40 h-40 rounded-full"
                    style={{ background: "radial-gradient(circle, hsla(43,100%,55%,0.12), transparent 70%)" }} />

                  <div className="relative z-10">
                    <blockquote className="text-xl font-bold text-white leading-relaxed mb-6">
                      "Money management should feel like <span className="text-primary">chatting with a friend</span> — natural, fast, and zero anxiety."
                    </blockquote>
                    <div className="flex items-center gap-3">
                      <div className="w-10 h-10 rounded-xl flex items-center justify-center font-black text-black text-sm"
                        style={{ background: "hsl(43 100% 55%)" }}>A</div>
                      <div>
                        <div className="text-sm font-bold text-white">Arjun Mehta</div>
                        <div className="text-xs text-muted-foreground">CEO & Co-founder, FluxPay</div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </Reveal>
          </div>
        </div>
      </section>

      <section className="py-24 px-6 relative" style={{ background: "hsl(0 0% 6%)" }}>
        <div className="max-w-6xl mx-auto">
          <Reveal direction="up" className="text-center mb-16">
            <div className="text-xs font-semibold tracking-widest mb-4" style={{ color: "hsl(43 100% 55%)" }}>WHAT WE BELIEVE</div>
            <h2 className="text-4xl md:text-5xl font-black text-white">Our core values</h2>
          </Reveal>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-5">
            {values.map((v, i) => <ValueCard key={i} {...v} />)}
          </div>
        </div>
      </section>

      <section className="py-24 px-6 relative overflow-hidden">
        <Orb size="500px" top="30%" left="-10%" color="hsla(260,80%,60%,0.06)" delay={2} blur={120} />
        <div className="max-w-5xl mx-auto">
          <div className="grid md:grid-cols-2 gap-16 items-start">
            <div className="md:sticky md:top-24">
              <Reveal delay={0.1}>
                <div className="text-xs font-semibold tracking-widest mb-4" style={{ color: "hsl(43 100% 55%)" }}>OUR JOURNEY</div>
              </Reveal>
              <Reveal delay={0.2}>
                <h2 className="text-4xl md:text-5xl font-black text-white leading-tight mb-6">
                  From idea to <span className="text-primary">10 million</span> users
                </h2>
              </Reveal>
              <Reveal delay={0.3}>
                <p className="text-muted-foreground leading-relaxed mb-8">
                  Every great product starts with a frustration. Ours started in a hostel room in Bangalore, 2020.
                </p>
              </Reveal>
              <Reveal delay={0.4}>
                <div className="rounded-2xl p-5 inline-block"
                  style={{ background: "hsl(0 0% 8%)", border: "1px solid hsla(43,100%,55%,0.15)" }}>
                  <div className="text-2xl font-black gradient-text mb-1">4 Years</div>
                  <div className="text-xs text-muted-foreground">of shipping, learning, and growing</div>
                </div>
              </Reveal>
            </div>

            <div className="space-y-0">
              {timeline.map((item, i) => (
                <TimelineItem key={i} {...item} isLast={i === timeline.length - 1} />
              ))}
            </div>
          </div>
        </div>
      </section>

      <section className="py-24 px-6 relative" style={{ background: "hsl(0 0% 6%)" }}>
        <div className="max-w-6xl mx-auto">

          <Reveal delay={0.3} className="mt-12">
            <div className="rounded-3xl p-8 md:p-10 text-center relative overflow-hidden"
              style={{ background: "linear-gradient(135deg, hsl(0 0% 9%), hsl(30 15% 11%))", border: "1px solid hsla(43,100%,55%,0.15)" }}>
              <div className="absolute inset-0 opacity-[0.03] rounded-3xl" style={{
                backgroundImage: `linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)`,
                backgroundSize: "24px 24px",
              }} />
              <div className="absolute -top-20 left-1/2 -translate-x-1/2 w-60 h-60 rounded-full"
                style={{ background: "radial-gradient(circle, hsla(43,100%,55%,0.08), transparent 70%)" }} />
              <div className="relative z-10">
                <div className="text-4xl mb-4">🚀</div>
                <h3 className="text-2xl font-black text-white mb-3">We're hiring!</h3>
                <p className="text-muted-foreground mb-6 max-w-md mx-auto">
                  Join a team that ships fast, thinks big, and genuinely cares about building something India will love.
                </p>
                <div className="flex flex-wrap gap-3 justify-center mb-8">
                  {["Frontend Engineer", "Product Designer", "Backend Engineer", "Growth Marketer", "Data Scientist"].map((role, i) => (
                    <span key={i} className="px-3 py-1.5 rounded-full text-xs font-medium"
                      style={{ background: "hsla(43,100%,55%,0.1)", border: "1px solid hsla(43,100%,55%,0.2)", color: "hsl(43 100% 65%)" }}>
                      {role}
                    </span>
                  ))}
                </div>
                <button className="px-8 py-3.5 rounded-full font-bold text-sm text-black transition-all hover:scale-105"
                  style={{ background: "hsl(43 100% 55%)", boxShadow: "0 8px 32px hsla(43,100%,55%,0.35)" }}>
                  View Open Roles →
                </button>
              </div>
            </div>
          </Reveal>
        </div>
      </section>


      <section className="py-32 px-6 relative overflow-hidden">
        <Orb size="700px" top="50%" left="50%" color="hsla(43,100%,55%,0.08)" delay={0} blur={150} />
        <div className="absolute inset-0 pointer-events-none" style={{
          backgroundImage: "radial-gradient(circle, rgba(255,255,255,0.05) 1px, transparent 1px)",
          backgroundSize: "40px 40px",
        }} />
        <div className="max-w-3xl mx-auto text-center relative z-10">
          <Reveal delay={0.1}>
            <h2 className="text-5xl md:text-6xl font-black text-white leading-tight mb-6">
              Ready to experience the <span className="gradient-text">future of money?</span>
            </h2>
          </Reveal>
          <Reveal delay={0.2}>
            <p className="text-muted-foreground text-lg mb-10 max-w-xl mx-auto">
              Join 10 million Indians who've already made the switch. No bank account needed. Takes 2 minutes.
            </p>
          </Reveal>
          <Reveal delay={0.3}>
            <div className="flex flex-wrap gap-4 justify-center">
              <button className="px-10 py-4 rounded-full font-bold text-black text-sm transition-all hover:scale-105 active:scale-95"
                style={{ background: "hsl(43 100% 55%)", boxShadow: "0 12px 40px hsla(43,100%,55%,0.4)" }}>
                Get Started — It's Free
              </button>
              <button className="px-10 py-4 rounded-full font-bold text-white text-sm transition-all hover:scale-105"
                style={{ background: "hsla(0,0%,100%,0.06)", border: "1px solid hsla(0,0%,100%,0.1)" }}>
                Download App
              </button>
            </div>
          </Reveal>
        </div>
      </section>

    </div>
  );
};

export default About;