import { useNavigate } from "react-router-dom";

const NotFound = () => {
  const navigate = useNavigate();
  return (
    <div className="text-center">
      <h2 className="text-2xl font-bold mb-4">Post not found</h2>
      <button className="px-4 py-2 bg-primary text-white rounded" onClick={() => navigate('/')}>
        Back to blog
      </button>
    </div>
  )
}

export default NotFound
