module Ode where

-- Function for backward Euler ODE integration
euler :: (RealFloat a, Show a) => (a -> a -> a) -> a -> a -> a -> a -> [(a,a)]
euler f t0 y0 tf dt =
  (t0, y0) : if t0 < tf then euler f (t0 + dt) (y0 + f t0 y0 * dt) tf dt
    else []

-- Function for Runge--Kutta 4 ODE integration
rk4 :: (RealFloat a, Show a) => (a -> a -> a) -> a -> a -> a -> a -> [(a,a)]
rk4 f t0 y0 tf dt =
  (t0, y0) : if t0 < tf then rk4 f (t0+dt) (y0+dy) tf dt
    else []
      where
        k1 = dt * f t0 y0
        k2 = dt * f (t0+dt/2.0) (y0+k1/2.0)
        k3 = dt * f (t0+dt/2.0) (y0+k2/2.0)
        k4 = dt * f (t0+dt) (y0+k3)
        dy = k1/6.0+k2/3.0+k3/3.0+k4/6.0
