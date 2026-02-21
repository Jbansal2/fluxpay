import React from "react";

const messages = [
  {
    id: 1,
    text: "Hey! I lost my FluxCard, 😅 what will I do now?",
    sender: "user",
    name: "Teresa",
    avatar: "T",
    avatarBg: "linear-gradient(135deg, #f97316, #ef4444)",
    position: "right",
  },
  {
    id: 2,
    text: "Hi! 👋 You can block your card instantly from the app. No transactions will be processed once your card is blocked as it gets deactivated permanently.",
    sender: "support",
    name: "Shifa",
    avatar: "S",
    avatarBg: "linear-gradient(135deg, #8b5cf6, #6366f1)",
    position: "left",
  },
  {
    id: 3,
    text: "OKK! Thank you sm. 🙏 Blocking the card right away",
    sender: "user",
    name: "Teresa",
    avatar: "T",
    avatarBg: "linear-gradient(135deg, #f97316, #ef4444)",
    position: "right",
  },
];

const SupportSection = () => {
  return (
    <section
      className="w-full py-20 px-6 relative overflow-hidden"
      style={{ background: "hsl(0 0% 6%)" }}
    >
      <div
        className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[700px] h-[400px] rounded-full pointer-events-none"
        style={{
          background:
            "radial-gradient(ellipse, hsla(43,100%,55%,0.03) 0%, transparent 70%)",
        }}
      />

      <div className="max-w-6xl mx-auto flex flex-col md:flex-row items-center gap-16 md:gap-20">

        <div className="flex-1 flex flex-col gap-4 max-w-md w-full">
          {messages.map((msg, i) => (
            <div
              key={msg.id}
              className={`flex items-end gap-2 ${
                msg.position === "right"
                  ? "flex-row-reverse ml-auto"
                  : "flex-row"
              } animate-message`}
              style={{
                animationDelay: `${i * 0.25}s`,
              }}
            >
              <div
                className="relative max-w-[280px] px-4 py-3 rounded-2xl"
                style={{
                  background: "hsl(0 0% 96%)",
                  borderRadius:
                    msg.position === "right"
                      ? "18px 18px 4px 18px"
                      : "18px 18px 18px 4px",
                  boxShadow: "0 4px 20px hsla(0,0%,0%,0.3)",
                }}
              >
                <p className="text-[13px] text-gray-800 leading-relaxed">
                  {msg.text}
                </p>

                <div className="flex items-center gap-1.5 mt-2 justify-end">
                  <span className="text-[10px] text-gray-400">
                    {msg.name}
                  </span>
                  <div
                    className="w-6 h-6 rounded-full flex items-center justify-center text-white text-[9px] font-bold shrink-0"
                    style={{ background: msg.avatarBg }}
                  >
                    {msg.avatar}
                  </div>
                </div>
              </div>
            </div>
          ))}
        </div>

        <div className="flex-1 max-w-lg">
          <h2 className="text-4xl md:text-5xl font-bold leading-tight mb-6 text-white">
            Our support team is{" "}
            <span className="text-primary">active 24x7</span>
          </h2>

          <p className="text-base text-muted-foreground leading-relaxed mb-10">
            Feel free to chat with our support team whenever you need more clarity
          </p>

          <button
            className="px-8 py-4 rounded-full text-base font-bold text-black transition-all duration-200 hover:scale-105 active:scale-95"
            style={{
              background: "hsl(43 100% 55%)",
              boxShadow: "0 8px 32px hsla(43,100%,55%,0.3)",
            }}
          >
            Contact Us
          </button>
        </div>
      </div>

      <style>{`
        .animate-message {
          opacity: 0;
          transform: translateY(20px) scale(0.96);
          animation: messageReveal 0.6s cubic-bezier(0.22, 1, 0.36, 1) forwards;
        }

        @keyframes messageReveal {
          to {
            opacity: 1;
            transform: translateY(0) scale(1);
          }
        }
      `}</style>
    </section>
  );
};

export default SupportSection;