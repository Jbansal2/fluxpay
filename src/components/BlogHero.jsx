import { featuredPost, recentPosts } from "../lib/blogData";
import BlogCard from "./BlogCard";
import { NavLink as RouterNavLink } from "react-router-dom";

const categories = ["All", "Finance", "Payments", "Savings", "Investing", "Tips & Tricks"];

const BlogHero = () => {
  return (
    <section
      className="w-full min-h-screen px-6 pt-24 pb-16 relative overflow-hidden"
      style={{ background: "hsl(0 0% 4%)" }}
    >
      <div
        className="absolute top-0 left-1/2 -translate-x-1/2 w-[800px] h-[400px] pointer-events-none"
        style={{ background: "radial-gradient(ellipse at 50% 0%, hsla(43,100%,55%,0.08) 0%, transparent 70%)" }}
      />

      <div className="max-w-6xl mx-auto">
        <div className="text-center mb-12">
          <div
            className="inline-flex items-center gap-2 px-4 py-1.5 rounded-full text-xs font-semibold text-primary mb-6"
            style={{ background: "hsla(43,100%,55%,0.1)", border: "1px solid hsla(43,100%,55%,0.2)" }}
          >
            ✦ FluxPay Blog
          </div>
          <h1 className="text-4xl md:text-6xl font-black font-display text-foreground leading-tight mb-4">
            Insights for the{" "}
            <span className="text-primary">future</span> of finance
          </h1>
          <p className="text-base text-muted-foreground max-w-xl mx-auto">
            Tips, guides, and stories to help you take control of your financial life.
          </p>
        </div>

        <div className="flex flex-wrap gap-2 justify-center mb-14">
          {categories.map((cat, i) => (
            <button
              key={cat}
              className="px-4 py-1.5 rounded-full text-sm font-medium transition-all duration-200 cursor-pointer"
              style={{
                background: i === 0 ? "hsl(43 100% 55%)" : "hsla(0,0%,100%,0.05)",
                color: i === 0 ? "black" : "hsl(0 0% 60%)",
                border: i === 0 ? "none" : "1px solid hsla(0,0%,100%,0.08)",
              }}
            >
              {cat}
            </button>
          ))}
        </div>

        <RouterNavLink to={`/blog/${featuredPost.title.toLowerCase().replace(/[^a-z0-9\s-]/g, '').trim().replace(/\s+/g, '-')}`} className="block">
        <div
          className="w-full rounded-3xl p-8 md:p-10 mb-8 relative overflow-hidden group"
          style={{
            background: "linear-gradient(135deg, hsl(0 0% 9%), hsl(30 20% 10%))",
            border: "1px solid hsla(43,100%,55%,0.15)",
            boxShadow: "0 0 60px hsla(43,100%,55%,0.06)",
          }}
        >
          <div
            className="absolute inset-0 opacity-[0.03] rounded-3xl"
            style={{
              backgroundImage: `linear-gradient(hsla(43,100%,55%,1) 1px, transparent 1px), linear-gradient(90deg, hsla(43,100%,55%,1) 1px, transparent 1px)`,
              backgroundSize: "40px 40px",
            }}
          />
          <div
            className="absolute -top-20 -right-20 w-64 h-64 rounded-full pointer-events-none"
            style={{ background: "radial-gradient(circle, hsla(43,100%,55%,0.08), transparent 70%)" }}
          />

          <div className="relative z-10 flex flex-col md:flex-row md:items-center gap-8">
            {/* Left: text */}
            <div className="flex-1">
              <div className="flex items-center gap-3 mb-4">
                <span
                  className="px-3 py-1 rounded-full text-[11px] font-bold text-black"
                  style={{ background: "hsl(43 100% 55%)" }}
                >
                  {featuredPost.tag}
                </span>
                <span
                  className="px-3 py-1 rounded-full text-[11px] font-medium text-muted-foreground"
                  style={{ background: "hsla(0,0%,100%,0.06)", border: "1px solid hsla(0,0%,100%,0.08)" }}
                >
                  {featuredPost.category}
                </span>
              </div>

              <h2 className="text-2xl md:text-3xl font-bold font-display text-foreground leading-snug mb-3 group-hover:text-primary transition-colors duration-200">
                {featuredPost.title}
              </h2>
              <p className="text-sm text-muted-foreground leading-relaxed mb-6 max-w-xl">
                {featuredPost.excerpt}
              </p>

              <div className="flex items-center gap-3">
                <div
                  className="w-8 h-8 rounded-full flex items-center justify-center text-black text-xs font-bold"
                  style={{ background: "hsl(43 100% 55%)" }}
                >
                  {featuredPost.authorInitials}
                </div>
                <div>
                  <div className="text-xs font-semibold text-foreground">{featuredPost.author}</div>
                  <div className="text-[10px] text-muted-foreground">{featuredPost.date} · {featuredPost.readTime}</div>
                </div>
                <div className="ml-auto">
                  <div
                    className="w-10 h-10 rounded-full flex items-center justify-center text-primary group-hover:scale-110 transition-transform"
                    style={{ background: "hsla(43,100%,55%,0.1)", border: "1px solid hsla(43,100%,55%,0.2)" }}
                  >
                    <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                      <path d="M3 8H13M9 4L13 8L9 12" stroke="hsl(43 100% 55%)" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round" />
                    </svg>
                  </div>
                </div>
              </div>
            </div>

            <div
              className="shrink-0 w-full md:w-64 h-40 rounded-2xl flex items-center justify-center relative overflow-hidden"
              style={{
                background: "linear-gradient(135deg, hsla(43,100%,55%,0.08), hsla(43,100%,55%,0.02))",
                border: "1px solid hsla(43,100%,55%,0.1)",
              }}
            >
              <div className="absolute top-4 left-4 w-16 h-16 rounded-full opacity-20"
                style={{ background: "radial-gradient(circle, hsl(43 100% 55%), transparent)" }} />
              <div className="absolute bottom-4 right-4 w-12 h-12 rounded-full opacity-10"
                style={{ background: "radial-gradient(circle, hsl(43 100% 55%), transparent)" }} />
              <div className="relative z-10 flex flex-col items-center gap-2">
                <div
                  className="w-14 h-14 rounded-2xl flex items-center justify-center text-2xl font-black text-black"
                  style={{ background: "hsl(43 100% 55%)" }}
                >
                  💡
                </div>
                <span className="text-xs text-muted-foreground">Featured Article</span>
              </div>
            </div>
          </div>
        </div>
        </RouterNavLink>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-5">
          {recentPosts.map((post) => (
            <BlogCard key={post.title} post={post} />
          ))}
        </div>

      </div>
    </section>
  );
};

export default BlogHero;