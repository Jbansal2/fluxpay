import React from 'react'

const Button = ({ title }) => {
    return (
        <button className="bg-primary cursor-pointer text-primary-foreground px-4 py-2 rounded-lg text-sm font-medium flex items-center gap-1.5 hover:opacity-90 transition-opacity">
            {title}
        </button>
    )
}

export default Button
