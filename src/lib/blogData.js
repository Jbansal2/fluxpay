const slugify = (s) =>
  s
    .toLowerCase()
    .replace(/[^a-z0-9\s-]/g, "")
    .trim()
    .replace(/\s+/g, "-");

export const featuredPost = {
  category: "Finance",
  title: "How to manage your money smarter in 2025",
  excerpt:
    "Learn the best strategies to budget, save, and grow your wealth with FluxPay's powerful financial tools designed for the next generation.",
  author: "Rahul Sharma",
  authorInitials: "R",
  date: "Feb 18, 2025",
  readTime: "5 min read",
  tag: "Featured",
  content:
    `Managing money starts with clear goals. In this article we cover budgeting, saving, and investment basics that are simple to follow.\n\nStart by tracking your expenses...`,
};

export const recentPosts = [
  {
    category: "Payments",
    title: "UPI payments just got faster — here's what changed",
    excerpt:
      "The new UPI stack brings sub-second transfers and improved reliability for all users.",
    author: "Priya Mehta",
    authorInitials: "P",
    date: "Feb 15, 2025",
    readTime: "3 min read",
    avatarBg: "linear-gradient(135deg, #8b5cf6, #6366f1)",
    content:
      "The UPI upgrade reduces latency and improves routing. In this post we explain what changed and how it affects merchants and consumers.",
  },
  {
    category: "Savings",
    title: "5 habits that will grow your savings by 40%",
    excerpt: "Small daily changes that compound into massive financial freedom over time.",
    author: "Aditya Kumar",
    authorInitials: "A",
    date: "Feb 12, 2025",
    readTime: "4 min read",
    avatarBg: "linear-gradient(135deg, #10b981, #059669)",
    content: "Saving consistently matters. Here are five practical habits to adopt today.",
  },
  {
    category: "Investing",
    title: "Start investing at 18 — why early matters",
    excerpt: "The power of compounding is real. Here's how teens can start their investment journey today.",
    author: "Sneha Patel",
    authorInitials: "S",
    date: "Feb 10, 2025",
    readTime: "6 min read",
    avatarBg: "linear-gradient(135deg, #ec4899, #f59e0b)",
    content: "Investing early gives you time for compounding. This guide covers basic first steps.",
  },
];

export const allPosts = [
  { ...featuredPost, slug: slugify(featuredPost.title) },
  ...recentPosts.map((p) => ({ ...p, slug: slugify(p.title) })),
];

export default { featuredPost, recentPosts, allPosts };
