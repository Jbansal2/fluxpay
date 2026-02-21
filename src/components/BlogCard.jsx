import { NavLink as RouterNavLink } from "react-router-dom";

const BlogCard = ({ post }) => {
  return (
    <RouterNavLink
      to={`/blog/${post.slug}`}
      className="rounded-2xl p-6 group transition-all duration-200 hover:-translate-y-1 block"
      style={{
        background: "hsl(0 0% 8%)",
        border: "1px solid hsla(0,0%,100%,0.06)",
        boxShadow: "0 4px 24px hsla(0,0%,0%,0.3)",
      }}
    >
      <span
        className="inline-block px-2.5 py-0.5 rounded-full text-[10px] font-semibold text-primary mb-4"
        style={{ background: "hsla(43,100%,55%,0.1)", border: "1px solid hsla(43,100%,55%,0.15)" }}
      >
        {post.category}
      </span>

      <h3 className="text-sm font-bold font-display text-foreground leading-snug mb-2 group-hover:text-primary transition-colors duration-200">
        {post.title}
      </h3>
      <p className="text-xs text-muted-foreground leading-relaxed mb-5">{post.excerpt}</p>

      <div className="flex items-center gap-2">
        <div
          className="w-7 h-7 rounded-full flex items-center justify-center text-white text-[10px] font-bold shrink-0"
          style={{ background: post.avatarBg || "linear-gradient(135deg,#64748b,#475569)" }}
        >
          {post.authorInitials}
        </div>
        <div>
          <div className="text-[10px] font-semibold text-foreground">{post.author}</div>
          <div className="text-[9px] text-muted-foreground">{post.date} · {post.readTime}</div>
        </div>
        <div
          className="ml-auto w-7 h-7 rounded-full flex items-center justify-center opacity-0 group-hover:opacity-100 transition-opacity"
          style={{ background: "hsla(43,100%,55%,0.1)" }}
        >
          <svg width="12" height="12" viewBox="0 0 12 12" fill="none">
            <path d="M2 6H10M7 3L10 6L7 9" stroke="hsl(43 100% 55%)" strokeWidth="1.2" strokeLinecap="round" strokeLinejoin="round" />
          </svg>
        </div>
      </div>
    </RouterNavLink>
  );
};

export default BlogCard;
