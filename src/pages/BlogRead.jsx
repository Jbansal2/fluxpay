import { useParams, useNavigate } from "react-router-dom";
import { allPosts } from "../lib/blogData";

const BlogRead = () => {
  const { slug } = useParams();
  const navigate = useNavigate();
  const post = allPosts.find((p) => p.slug === slug);

  if (!post) {
    return (
      <section className="min-h-screen flex items-center justify-center px-6 pt-24">
        <div className="text-center">
          <h2 className="text-2xl font-bold mb-4">Post not found</h2>
          <button className="px-4 py-2 bg-primary text-white rounded" onClick={() => navigate('/blog')}>
            Back to blog
          </button>
        </div>
      </section>
    );
  }

  return (
    <section className="w-full min-h-screen px-6 pt-24 pb-16" style={{ background: "hsl(0 0% 4%)" }}>
      <div className="max-w-4xl mx-auto prose prose-invert">
        <div className="mb-6">
          <div className="inline-block px-3 py-1 rounded-full text-[11px] font-bold text-black" style={{ background: "hsl(43 100% 55%)" }}>{post.category}</div>
        </div>
        <h1 className="text-3xl font-extrabold mb-4">{post.title}</h1>
        <div className="text-sm text-muted-foreground mb-8">By {post.author} · {post.date} · {post.readTime}</div>

        <article className="text-base text-muted-foreground leading-relaxed">
          {post.content.split('\n\n').map((p, i) => (
            <p key={i} className="mb-4">{p}</p>
          ))}
        </article>
      </div>
    </section>
  );
};

export default BlogRead;
