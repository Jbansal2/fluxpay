import React from 'react'
import Navbar from '../components/Navbar'
import HeroSection from '../components/HeroSection'
import TrustBar from '../components/TrustBar'
import Stories from '../components/Stories'

const Home = () => {
return (
    <div className="min-h-screen bg-background">
      <Navbar />
      <HeroSection />
      <TrustBar />
      <Stories />
    </div>
  );
}

export default Home
