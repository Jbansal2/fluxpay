import React from 'react'

const TrustBar = () => {
    return (
        <div className="w-full bg-[#1a1a1a] py-6 px-4 sm:px-6 lg:px-8">
            <div className="max-w-7xl mx-auto flex flex-col lg:flex-row items-center gap-6 lg:gap-8">

                {/* Badge */}
                <div className="flex flex-col items-center text-white shrink-0">
                    <svg
                        className="w-20 sm:w-24 lg:w-[100px]"
                        viewBox="0 0 90 80"
                        fill="none"
                        xmlns="http://www.w3.org/2000/svg"
                    >
                        <path
                            d="M40 6 L70 18 L70 42 C70 57 56 67 40 74 C24 67 10 57 10 42 L10 18 Z"
                            stroke="#555"
                            strokeWidth="1"
                            fill="none"
                        />
                        <text x="40" y="38" textAnchor="middle" fill="white" fontSize="11" fontWeight="700" fontFamily="Inter, sans-serif">
                            10 million+
                        </text>
                        <text x="40" y="52" textAnchor="middle" fill="#999" fontSize="7" fontFamily="Inter, sans-serif">
                            users love FamX
                        </text>
                    </svg>
                </div>

                {/* Divider (only desktop) */}
                <div className="hidden lg:block h-12 w-px bg-gray-600 shrink-0" />

                {/* Logos Section */}
                <div className="flex flex-wrap lg:flex-nowrap items-center justify-center lg:justify-between gap-6 sm:gap-10 lg:gap-16 w-full">

                    <span className="text-gray-400 text-sm font-medium whitespace-nowrap">
                        Powered by
                    </span>

                    {/* Trio */}
                    <div className="flex items-center gap-1">
                        <span className="text-white text-xl sm:text-2xl font-bold tracking-tight">tri</span>
                        <span className="relative">
                            <span className="text-xl sm:text-2xl font-bold text-white">o</span>
                            <span className="absolute -top-1 left-1 flex gap-0.5">
                                <span className="w-1.5 h-1.5 rounded-full bg-orange-500 inline-block" />
                                <span className="w-1.5 h-1.5 rounded-full bg-green-500 inline-block" />
                            </span>
                        </span>
                    </div>

                    {/* VISA */}
                    <div className="text-white text-xl sm:text-2xl font-black italic tracking-widest">
                        VISA
                    </div>

                    {/* RuPay */}
                    <div className="flex items-center gap-1">
                        <span className="text-white text-lg sm:text-xl font-bold italic">RuPay</span>
                        <svg width="18" height="18" viewBox="0 0 20 20" fill="none">
                            <polygon points="10,2 18,10 10,18 2,10" fill="#f97316" />
                            <polygon points="14,2 20,10 14,18 8,10" fill="#ea580c" opacity="0.7" />
                        </svg>
                    </div>

                    {/* UPI */}
                    <div className="flex items-center">
                        <svg
                            className="w-36 sm:w-40"
                            viewBox="0 0 48 28"
                            fill="none"
                        >
                            <rect width="48" height="28" rx="3" fill="#1a1a1a" />
                            <text x="4" y="18" fill="white" fontSize="11" fontWeight="800" fontFamily="Inter, sans-serif" letterSpacing="1">
                                UPI
                            </text>
                            <polygon points="36,8 44,14 36,20 38,14" fill="#f97316" />
                            <polygon points="30,8 38,14 30,20 32,14" fill="#22c55e" />
                            <text x="4" y="26" fill="#888" fontSize="5" fontFamily="Inter, sans-serif">
                                UNIFIED PAYMENTS INTERFACE
                            </text>
                        </svg>
                    </div>
                </div>
            </div>
        </div>
    );
}

export default TrustBar