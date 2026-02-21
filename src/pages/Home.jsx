import React from 'react'
import Navbar from '../components/Navbar'
import HeroSection from '../components/HeroSection'
import TrustBar from '../components/TrustBar'
import Stories from '../components/Stories'
import OneAccount from '../components/OneAccount'
import FluxPayTeens from '../components/FluxPayTeens'
import UPISection from '../components/UPISection'
import FluxXCard from '../components/FluxXCard'
import SupportSection from '../components/SupportSection'

const Home = () => {
return (
    <div className="min-h-screen bg-background">
      <Navbar />
      <HeroSection />
      <TrustBar />
      <OneAccount />
      <FluxPayTeens />
      <UPISection />
      <FluxXCard />
      <SupportSection />
      <Stories />

    </div>
  );
}

export default Home
