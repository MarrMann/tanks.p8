pico-8 cartridge // http://www.pico-8.com
version 42
__lua__
// general

wait_t = 0
t = 0

function _init()
	init_world()
	load_room(0,0)
end

function _update()
	if wait_t > 0 then
		wait_t -= 1
		return
	end
	t+=1
	handle_input()
	update_world()
	update_player()
	update_shots()
	update_particles()
	update_explosions()
end	

function _draw()
	cls()
	draw_world()
	draw_player()
	draw_shots()
	draw_particles()
	draw_explosions()
	draw_types()
end

function lerp(a,b,t)
 local result=a+t*(b-a)
 return result
end
	
-->8
// player stuff

p = {
	x = 64,
	y = 64,
	move = false,
	spr = 0,
	t_angle = 0,
	pow = 0.5,
	is_grounded = false
}

x_hold_frames = 0
x_tap = 7

function handle_input()
	local prev_x = p.x
	local angle = 0
	local fl_x = flr(p.x)
	local fl_y = flr(p.y)
	p.move = false
	
	// movement
	if btn(⬅️) then
		local target_x = p.x-1
		local target_y = get_ground(flr(target_x),fl_y)
		local angle = atan2(-1,(fl_y-target_y))
		
		//printh('angle: '..angle,'dbg.txt')
		if (angle > 0.25 and angle < 0.7) then
			p.x += cos(angle) * 0.5
			p.y -= sin(angle) * 0.5
		end
	end
	if btn(➡️) then
		local target_x = p.x+1
		local target_y = get_ground(flr(target_x),fl_y)
		local angle = atan2(1,(fl_y-target_y))
		if (angle < 0.25 or angle > 0.8) then
		 p.x += cos(angle) * 0.5
		 p.y -= sin(angle) * 0.5
	 end
	end
	if btn(❎) then //holding ❎
		x_hold_frames += 1
	elseif x_hold_frames > 0 then
		if x_hold_frames <= x_tap then
			local dx = sin(p.t_angle)
			local dy = cos(p.t_angle)
			local ox = p.x+dx*4
			local oy = p.y-3+dy*4
			shoot(ox,oy,dx,dy,p.pow*6+1)
			sfx(1)
		end
		x_hold_frames = 0
	end
	
	if x_hold_frames > x_tap then
		if btn(⬆️) then p.pow += 0.02 end
		if btn(⬇️) then p.pow -= 0.02 end
		p.pow=min(1.0,max(0.0,p.pow))
		if btn(⬆️) or btn(⬇️) then
			sfx(0, 3, p.pow*31, 2)
		end
	else
		if btn(⬆️) then p.t_angle += 0.01 end
		if btn(⬇️) then p.t_angle -= 0.01 end
	end
	
	if prev_x != p.x then
		p.move = true
	end
end

function get_ground(x, y)
	local c = 0
	while(mapget(x,y) == 0 and c < 128) do
		y += 1
		c += 1
	end
	
	while(mapget(x,y) != 0 and c < 128) do
		y -= 1
	end
	
	return y
end

function draw_dbg()

end

function update_player()
	// gravity
	if not p.is_grounded then
		if mapget(p.x, p.y+1) == 0 then
			p.y += 1
		else
			p.is_grounded = true
		end
	end
end

function draw_player()
	draw_dbg()
	
	// player sprite
	local tur_x = sin(p.t_angle)*4
	local tur_y = cos(p.t_angle)*4
	if p.move then p.spr = (t%4)/2 end
	//pset(p.x,p.y,7)
	p.x -= 4
	p.y -= 6
	spr(p.spr, p.x, p.y)
	p.x += 4
	p.y += 6
	local t_end_x=p.x+tur_x
	local t_end_y=p.y-3+tur_y
	line(p.x, p.y-3, t_end_x, t_end_y, 7)
	
 // crosshair
 local cross_x = p.x+tur_x*8
 local cross_y = p.y-3+tur_y*8
 line(cross_x-2,cross_y,cross_x-4,cross_y,7)
 line(cross_x+2,cross_y,cross_x+4,cross_y,7)
 line(cross_x,cross_y-2,cross_x,cross_y-4,7)
 line(cross_x,cross_y+2,cross_x,cross_y+4,7)
 
 // power
 if btn(❎) and x_hold_frames > x_tap then
	 local pow_col = 8
	 if p.pow > 0.33 then pow_col=9 end
	 if p.pow > 0.66 then pow_col=11 end
	 local pow_ox = p.x+tur_x*2
	 local pow_oy = p.y-3+tur_y*2
	 local dest_x = lerp(pow_ox,cross_x,p.pow)
	 local dest_y = lerp(pow_oy,cross_y,p.pow)
	 line(pow_ox,pow_oy,dest_x,dest_y,pow_col)
	end
end
-->8
// world stuff

// falling pixels array
fallings = {}
to_del = {}

function init_world()
	cls()
	memset(0x8000, 0x00, 0x2000)
	map()
	memcpy(0x8000, 0x6000, 0x2000)
end

function update_world()
	for i=#fallings,1,-1 do
		// make column fall
		local p=fallings[i]
		fallcolumn(p.x, p.y)
		
		// update column for next fall
		p.y += 1
		fallings[i]=p
		
		// column hit ground
		if mapget(p.x,p.y+1) != 0 or p.y > 126 then
			del(fallings, p)
		end
	end
end

function draw_world()
	memcpy(0x6000, 0x8000, 8000)
end

function fallcolumn(x, y)
	startcol = mapget(x, y+1)
	col = mapget(x, y)
	while col != 0 do
		mapset(x, y+1, col)
		y -= 1
		col = mapget(x,y)
	end
	mapset(x, y+1, startcol)
end

function mapget(x, y)
	if (y < 0 or x < 0 or x > 127 or y > 127) return 0
	local offset = (x + y*128) / 2
	local memloc = 0x8000 + flr(offset)
	if x % 2 == 0 then
		memloc = @memloc	& 0b00001111
	else
		memloc = (@memloc & 0b11110000) >> 4
	end
	return memloc
end

function mapset(x, y, col)
	if (y < 0 or x < 0 or x > 127 or y > 127) return
	local offset = (x + y*128) / 2
	local memloc = 0x8000 + flr(offset)
	local value = @memloc
	if x % 2 == 0 then
		mask = 0b11110000
		value = (value & mask) | col
		// add color
	else
		mask = 0b00001111
		value = (value & mask) | (col << 4)
		// add color
	end
	poke(memloc, value) 
end

function draw_circ(o_x, o_y, x, y, fall)
	y1 = o_y+y
	y2 = o_y-y
	y3 = o_y+x
	y4 = o_y-x
	for i = o_x-x,o_x+x do
		mapset(i, y1, 0)
		mapset(i, y2, 0)
	end
	for i = o_x-y,o_x+y do
		mapset(i, y3, 0)
		mapset(i, y4, 0)
	end
	if fall then
		if mapget(o_x+x, y2 - 1) != 0 then
			add(fallings, {x=o_x+x, y=y2-1})
		end
		if mapget(o_x+y, y4 - 1) != 0 then
			add(fallings, {x=o_x+y, y=y4-1})
		end
		if mapget(o_x-x, y2 - 1) != 0 then
			add(fallings, {x=o_x-x, y=y2-1})
		end
		if mapget(o_x-y, y4 - 1) != 0 then
			add(fallings, {x=o_x-y, y=y4-1})
		end
	end
end

function explode_brem(o_x, o_y, r, fall)
	x = 0
	y = r
	d = 3 - 2 * r
	draw_circ(o_x, o_y, x, y, fall)
	while y >= x do
		if d > 0 then
			y -= 1
			d = d + 4 * (x - y) + 10
		else
			d = d + 4 * x + 6
		end
		
		x += 1
		draw_circ(o_x, o_y, x, y, fall)
	end
end
-->8
// shooting & collisions

// shot={
// 	x,
// 	y,
// 	xvel,
// 	yvel,
// 	xprev,
//		yprev
// }
shots = {}
grav = 0.2

function update_shots()
	for i=#shots,1,-1 do
		s = shots[i]
		
		// hit info
		local inf = checkcol(s)
		if (inf.didhit) then
			if (inf.prevx == inf.x and inf.prevy == inf.y) then
				inf.prevx = s.xprev
				inf.prevy = s.yprev
			end
			local target=did_hit_target(inf.x,inf.y)
			if target!=nil then
				add(hit_texts,{
					x=inf.x,
					y=inf.y,
					col=10,
					life=30,
					text="+1"
				})
				del(targets,target)
			end
			
			ex_x=flr(inf.x)
			ex_y=flr(inf.y)
			hit_col=mapget(ex_x,ex_y)
		 add(exps, {x=ex_x, y=ex_y, r=2, cur_r=2})
		 
		 // map/dirt particles
		 n_x,n_y=get_q_normal(ex_x, ex_y)
		 p_x=ex_x+n_x*2
		 p_y=ex_y+n_y*2
		 for k=1,5 do
		 	add(map_parts, {
		 		x=flr(p_x),
				 y=flr(p_y),
				 xvel=rnd(5.0) - 2.5,
				 yvel=rnd(5.0) - 2.5,
				 xprev=flr(inf.prevx),
				 yprev=flr(inf.prevy),
				 life=120,
				 col=hit_col
		 	})
		 end
		end
		
		// particles
		for j=1,2 do
			add(parts, {
				x=s.x,
				y=s.y,
				xvel=rnd(1.5) - 0.75,
				yvel=rnd(1.5) - 0.75,
				xprev=s.x,
				yprev=s.y,
				life=rnd(10)+5,
				col=8+j
			})
		end
		
		// update
		s.x += s.xvel
		s.y += s.yvel
		s.yvel += grav
		if inf.didhit or s.y > 128 or s.x > 128 or s.x < 0 then
			del(shots, s)
		end
	end
end

function draw_shots()
	for i=#shots,1,-1 do
		s = shots[i]
		local dx = (s.x+s.xvel) - s.x
		local dy = (s.y+s.yvel) - s.y
		local d = sqrt(dx*dx+dy*dy)
		dy /= d
		dx /= d
		pset(s.x-dx,s.y-dy,8)
		line(s.x,s.y,s.x+s.xvel,s.y+s.yvel,7)
	end
end

function shoot(ox, oy, dx, dy, vel)
	local s = {
		x = ox,
		y = oy,
		xvel = dx*vel,
		yvel = dy*vel
	}
	add(shots, s)
end
-->8
//exp = {
//	x=0,
//	y=0,
//	r=5,
//	cur_r=0
//}

exps={}

function update_explosions()
	for i=#exps,1,-1 do
		local exp = exps[i]
		eq = exp.cur_r == exp.r
		explode_brem(exp.x, exp.y, exp.cur_r, eq)
	end
end

function draw_explosions()
	for i=#exps,1,-1 do
		local exp = exps[i]
		circ(exp.x, exp.y, exp.cur_r, 8)
		exp.cur_r += 1
		if exp.cur_r > exp.r then
			del(exps, exp)
		end
	end	
end
-->8
// particles

//particle = {
//	x,
//	y,
//	xvel,
//	yvel,
// xprev,
// yprev,
//	life,
//	col
//}

parts = {}
map_parts = {}

function update_particles()
	for i=#parts,1,-1 do
		local p=parts[i]
		p.x += p.xvel
		p.y += p.yvel
		p.life -= 1
	end
	
	for i=#map_parts,1,-1 do
		local mp=map_parts[i]
		
		local inf = checkcol(mp)
		if inf.didhit then
			hit_x=flr(inf.x)
			hit_y=flr(inf.y)
			n_x,n_y=get_q_normal(hit_x,hit_y)
			mp_x=hit_x+n_x
			mp_y=hit_y+n_y
			mapset(mp_x, mp_y, mp.col)
			add(fallings, {x=mp_x, y=mp_y})
		end
		
		mp.x += mp.xvel
		mp.y += mp.yvel
		mp.yvel += grav
		mp.life -= 1
		
		if inf.didhit or mp.y > 128 or mp.x > 128 or mp.x < 0 then
			del(map_parts, mp)
		end
	end
end

function draw_particles()
	for i=#parts,1,-1 do
		local p=parts[i]
		pset(p.x, p.y, p.col)
		if (p.life <= 0) del(parts, p)
	end
	
	for i=#map_parts,1,-1 do
		local mp=map_parts[i]
		pset(mp.x, mp.y, mp.col)
		line(mp.x,mp.y,mp.x+mp.xvel,mp.y+mp.yvel,mp.col)
		checkcol(mp)
		if (mp.life <= 0) del(map_parts, mp)
	end
end
-->8
// collisions
function checkcol(s)
	local x0 = s.x
	local y0 = s.y
	local x1 = s.x + s.xvel
	local y1 = s.y + s.yvel
	
	if abs(y1 - y0) < abs(x1 - x0) then
		return checkcollow(x0, y0, x1, y1)
	end
	return checkcolhigh(x0, y0, x1, y1)
end

function checkcollow(x0, y0, x1, y1)
 dx = x1 - x0
 dy = y1 - y0
 yi = sgn(dy)
 xi = sgn(dx)
 d = (2 * dy * yi) - dx*xi
 y = y0
 prevx = x0
 prevy = y

 for x=x0,x1,xi do
  if (mapget(flr(x),flr(y)) != 0) then
   local d = sqrt(dx*dx+dy*dy)
  	dx /= d
  	dy /= d
  	return {
	  	didhit=true,
	  	x=x,
	  	y=y,
	  	prevx=prevx,
	  	prevy=prevy
	  }
  end
  
 	if d > 0 then
 		y += yi
 		d += 2 * (dy*yi - dx*xi)
		else
			d += 2*dy*yi
		end
		
		prevx = x
		prevy = y
	end
	//check endpoint
	if (mapget(flr(x1),flr(y1)) != 0) then
		return {
	 	didhit=true,
	 	x=x1,
	 	y=y1,
	 	prevx=prevx,
	 	prevy=prevy
	 }
	end
 
	return {didhit=false}
end

function checkcolhigh(x0, y0, x1, y1)
 dx = x1 - x0
 dy = y1 - y0
 xi = sgn(dx)
 yi = sgn(dy)
 d = 2 * dx - dy*yi
 x = x0
 prevx = x
 prevy = y0
 
 for y=y0,y1,sgn(dy) do
  if (mapget(flr(x),flr(y)) != 0) then
  	local d = sqrt(dx*dx+dy*dy)
  	dx /= d
  	dy /= d
  	return {
	  	didhit=true,
	  	x=x,
	  	y=y,
	  	prevx=prevx,
	  	prevy=prevy
	  }
	 end
	 
 	if d > 0 then
 		x += xi
 		d += 2 * (dx*xi - dy*yi)
		else
			d += 2*dx*xi
		end
		
		prevx = x
		prevy = y
	end
	//check endpoint
	if (mapget(flr(x1),flr(y1)) != 0) then
		return {
	 	didhit=true,
	 	x=x1,
	 	y=y1,
	 	prevx=prevx,
	 	prevy=prevy
	 }
	end

	return {didhit=false}
end

-- get the terrain normal at (x, y)
function get_normal(x, y)
 local dx = 0
 local dy = 0

 for offset_y = -1, 1 do
  for offset_x = -1, 1 do
   if offset_x != 0 or offset_y != 0 then
     	
    local value = mapget(flr(x + offset_x), flr(y + offset_y))
    dx += offset_x * value
    dy += offset_y * value
   end
  end
 end

 -- flip gradient to get normal
 local normal_x = -dx
 local normal_y = -dy

 -- normalize the vector
 local length = sqrt(normal_x^2 + normal_y^2)
 if length > 0 then
  normal_x /= length
  normal_y /= length
 end

 return normal_x, normal_y
end

-- get quantized normal at (x, y)
function get_q_normal(x, y)
  local normal_x, normal_y = get_normal(x, y)
  local quantized_x = flr(normal_x + 0.5) -- round to nearest integer
  local quantized_y = flr(normal_y + 0.5) -- round to nearest integer
  return quantized_x, quantized_y
end
-->8
// map types and room loading
room = {x=0,y=0}
targets = {}
hit_texts = {}

target_t = 62

function load_room(x,y)
 // clear
	for t in all(targets) do
		del(targets, t)
	end
	
	room.x = x
	room.y = y
	
	for tx=0,15 do
		for ty=0,15 do
		 local tile = mget(room.x*16+tx,room.y*16+ty)
		 if tile==target_t then
		 	add(targets, {
		 		x=tx*8,
		 		y=ty*8
		 	})
		 end
		end
	end	
end

// hit_text:x,y,col,life,text
function draw_types()
	for ht in all(hit_texts) do
		print(ht.text,ht.x,ht.y,ht.col)
		ht.life -= 1
		ht.y -= 0.5
		ht.x += cos(ht.y*0.1)*0.5
		if ht.life < 0 then
			del(hit_texts, ht)
		end
	end
end

function did_hit_target(x,y)
	for t in all(targets) do
		if x>=t.x and x<=t.x+7 and y>=t.y	and y<=t.y+7 then
			return t
		end
	end
	return nil
end
__gfx__
00000000000000003333333344444444444440004000000000000000330000004000000000000004000000000000000000000000000000000000000000000000
00000000000000003344343444444444444440004000000000000000343300004400000000000044000000000000000000000000000000000000000000000000
00000000000000004444344444444444444444004400000000000000444333004440000000000444000000000000000000000000000000000000000000000000
000cc000000cc0004444444444444444444444004400000000000000444434334444000000004444000000000000000000000000000000000000000000000000
07cccc7007cccc704444444444444444444444404440000033000000444444434444400000044444000000000000000000000000000000000000000000000000
0cccccc00cccccc04444444444444444444444404440000034330000444444444444440000444444000000000000000000000000000000000000000000000000
15151515515151514444444444444444444444444444000044433300444444444444444004444444000000000000000000000000000000000000000000000000
51515151151515154444444444444444444444444444000044443433444444444444444444444444000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000044444444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008888888800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008777777800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008788887800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008787787800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008787787800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008788887800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008777777800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008888888800000000
__map__
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
133f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
133f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f3f09131313043f3f3f3f3f3f3f3f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f3f090303030313053f3f3f3f3f3f3f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033f3f09031303031313043f3f3f3f3f3f003f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0302021313130303030313083f3f3f3f3f003f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03130313031303031303031307063e033f3f3f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
031313130303030313130303031303033f3f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
131303030313131303030303031303033f3f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000003f3f3f3f000000000000003f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100000012002120041200512007120091200b1200c1200e1201012011120131201512017120181201a1201c1201d1201f1202112023120241202612028120291202b1202d1202f12030120321203412035120
570100000015003150061500915013150291501d150171501415011150101500c1500a15008150071500515002150001500010000100021000010000100011000010000100001000110000100001000010000100
