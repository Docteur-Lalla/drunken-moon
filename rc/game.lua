function new_bullet(fx, fy, fr, sk, dt)
  ball = { complex = true,
           x = fx,
           y = fy,
	   r = fr,
	   skin = sk,
	   life = dt }

  return ball
end

function game()
  hsPrint("heeey")

  local fx = function(t) return 2*t + 1 end
  local fy = function(t) return 3*t - 4 end
  local fr = function(t) return 3 end

  local bullet = new_bullet(fx, fy, fr, "bullet1", 1000)
end
