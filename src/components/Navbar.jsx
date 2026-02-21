import { useState } from "react";
import { ArrowRight, Menu, X } from "lucide-react";
import { Sheet, SheetContent, SheetTrigger, SheetTitle } from "./ui/sheet";
import logo from "../assets/logo.png";
import Button from "./Button";
import { NavLink as RouterNavLink } from "react-router-dom";

const navItems = [
    { label: "Home", to: "/" },
    { label: "Products", to: "/product" },
    { label: "About", to: "/about" },
    { label: "Blogs", to: "/blog" },
];

const Navbar = () => {
    const [open, setOpen] = useState(false);

    return (
        <nav className="fixed top-0 left-0 right-0 z-50 glass-card">
            <div className="max-w-7xl mx-auto px-6 h-16 flex items-center justify-between">
                <RouterNavLink to="/" className="flex items-center gap-1">
                    <img src={logo} alt="FluxPay Logo" className="w-15 h-10" />
                    <span className="font-display font-semibold text-lg text-foreground">FluxPay</span>
                </RouterNavLink>

                <div className="hidden md:flex items-center gap-8">
                    {navItems.map((item) => (
                        <RouterNavLink
                            key={item.label}
                            to={item.to}
                            className="text-sm text-muted-foreground hover:text-foreground transition-colors duration-200"
                        >
                            {item.label}
                        </RouterNavLink>
                    ))}
                </div>

                <div className="hidden md:flex items-center gap-4">
                    <Button title="Download" />
                </div>

                <div className="md:hidden">
                    <Sheet open={open} onOpenChange={setOpen}>
                        <SheetTrigger asChild>
                            <button className="p-2 text-foreground">
                                <Menu className="w-6 h-6" />
                            </button>
                        </SheetTrigger>
                        <SheetContent side="right" className="w-[280px] bg-background border-border">
                            <SheetTitle className="sr-only">Navigation Menu</SheetTitle>
                            <div className="flex flex-col gap-6 mt-8">
                                {navItems.map((item) => (
                                    <RouterNavLink
                                        key={item.label}
                                        to={item.to}
                                        onClick={() => setOpen(false)}
                                        className="text-lg text-muted-foreground hover:text-foreground transition-colors duration-200"
                                    >
                                        {item.label}
                                    </RouterNavLink>
                                ))}
                                <div className="h-px bg-border my-2" />
                                <button className="bg-primary text-primary-foreground px-4 py-3 rounded-lg text-sm font-medium flex items-center justify-center gap-1.5 hover:opacity-90 transition-opacity w-full">
                                    Download
                                </button>
                            </div>
                        </SheetContent>
                    </Sheet>
                </div>
            </div>
        </nav>
    );
};

export default Navbar;
