pico-8 cartridge // http://www.pico-8.com
version 43
__lua__
-- general

function _init()
	entity_id = 0
	gameover = false
	wait_t = 0
	t = 0
	p={
		id = get_entity_id(),
		x = 64,
		y = get_ground(64,64),
		xvel=0, -- used when knocked
		yvel=0,
		move = false,
		spr = 0,
		t_angle = 0,
		pow = 0.5,
		is_grounded = true,
		health=100,
		max_health=100,
		health_regen=10,
		is_knocked=false,
		cd=60,
		cur_cd=0,
		jumps = 3,
		shot = {
			name = shot_types.basic.name,
			modifiers = {
				size = { stacks = 6 }
			},
			child = nil
		}
	}
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
	update_enemies()
	update_shots()
	update_particles()
	update_explosions()
end

function _draw()
	cls()
	draw_world()
	draw_player()
	draw_enemies()
	draw_shots()
	draw_particles()
	draw_explosions()
	draw_types()
	draw_bounds()
	draw_ui()
end

function get_entity_id()
	entity_id += 1
	return entity_id
end

function lerp(a,b,t)
 local result=a+t*(b-a)
 return result
end

//https://www.lexaloffle.com/bbs/?tid=36059
function approx_dist(dx,dy)
 local maskx,masky=dx>>31,dy>>31
 local a0,b0=(dx+maskx)^^maskx,(dy+masky)^^masky
 if a0>b0 then
  return a0*0.9609+b0*0.3984
 end
 return b0*0.9609+a0*0.3984
end
	
-->8
-- player stuff

function handle_input()
	if gameover then
		if btn(🅾️) then
			_init()
		end
		return
	end

	local prev_x = p.x
	local angle = 0
	local fl_x = flr(p.x)
	local fl_y = flr(p.y)
	p.move = false

	-- movement
	if not p.is_knocked then
		if btn(⬅️) and (room_state.bounds==false or p.x > 1) then
			local target_x = p.x-1
			local target_y = get_ground(flr(target_x),fl_y)
			local angle = atan2(-1,(fl_y-target_y))
			if (angle > 0.25 and angle < 0.73) then
				p.x += cos(angle) * 0.5
				p.y -= sin(angle) * 0.5
			end
		end
		if btn(➡️) and (room_state.bounds==false or p.x < 127) then
			local target_x = p.x+1
			local target_y = get_ground(flr(target_x),fl_y)
			local angle = atan2(1,(fl_y-target_y))
			if (angle < 0.25 or angle > 0.77) then
				p.x += cos(angle) * 0.5
				p.y -= sin(angle) * 0.5
			end
		end
	end

	-- jumping
	if btnp(➡️,1) then
			if (p.jumps > 0 and p.is_grounded) then
			local vel = 4
			p.xvel = vel * cos(p.t_angle)
			p.yvel = vel	* sin(p.t_angle)
			p.is_grounded = false
			add(knocked_entities,p)
			p.jumps	-= 1
			sfx(1)
		end
	end

	-- shooting
	if btnp(🅾️) and p.cur_cd <= 0 then --z
		local cd = shoot_from_turret(p.x,p.y,p.t_angle,p.pow,p.id,p.shot)
		p.cur_cd = cd
		p.cd = cd
	end
	
	--power
	if btn(⬇️,1) then p.pow += 0.02 end --d
	if btn(❎) then p.pow -= 0.02 end --x
	p.pow=min(1.0,max(0.0,p.pow))
	if btn(⬇️,1) or btn(❎) then
		sfx(0, 3, p.pow*31, 2)
	end
	--angle
	if btn(⬆️) then p.t_angle += 0.01 end
	if btn(⬇️) then p.t_angle -= 0.01 end
	
	if prev_x != p.x then
		p.move = true
	end

	-- load next room at edges
	if not room_state.bounds then
		if p.x < 0 then
			p.x = 127
			room.x -= 1
			load_room(room.x, room.y)
		elseif	p.x > 127 then
			p.x = 0
			room.x += 1
			load_room(room.x, room.y)
		elseif	p.y > 127 then
			p.y = 0
			room.y += 1
			load_room(room.x, room.y)
		end
	end
end

function get_ground(x, y)
	if not room_state.bounds then
		x = mid(0, x, 127)
	end

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

-- keep for testing when needed
function draw_dbg()
end

function update_player()
	if (gameover) return

	-- gravity
	if not p.move and not p.is_knocked then
		local grnd = get_ground(flr(p.x),flr(p.y))
		if grnd > p.y then
			p.y += 1
			p.is_grounded = false
		else
			p.is_grounded = true
		end
	end
	if p.cur_cd>0 then
		p.cur_cd-=1
	end
end

function draw_player()
	if (gameover) return
	
	-- player sprite
	if p.move then p.spr = (t%4)/2 end
	p.x -= 4
	p.y -= 6
	spr(p.spr, p.x, p.y)
	p.x += 4
	p.y += 6

	-- turret
	local tur_x = cos(p.t_angle)*4
	local tur_y = sin(p.t_angle)*4
	local t_end_x=p.x+tur_x
	local t_end_y=p.y-3+tur_y
	line(p.x, p.y-3, t_end_x, t_end_y, 7)

 -- crosshair
 local cross_x = p.x+tur_x*8
 local cross_y = p.y-3+tur_y*8
 line(cross_x-2,cross_y,cross_x-4,cross_y,7)
 line(cross_x+2,cross_y,cross_x+4,cross_y,7)
 line(cross_x,cross_y-2,cross_x,cross_y-4,7)
 line(cross_x,cross_y+2,cross_x,cross_y+4,7)

 -- power
 if btn(⬇️,1) or btn(❎) then
	 local pow_col = 8
	 if p.pow > 0.33 then pow_col=9 end
	 if p.pow > 0.66 then pow_col=11 end
	 local pow_ox = p.x+tur_x*2
	 local pow_oy = p.y-3+tur_y*2
	 local dest_x = lerp(pow_ox,cross_x,p.pow)
	 local dest_y = lerp(pow_oy,cross_y,p.pow)
	 line(pow_ox,pow_oy,dest_x,dest_y,pow_col)
	end
	draw_dbg()
end
-->8
-- world stuff

-- falling pixels array
fallings = {}
to_del = {}

function init_world()
	cls()
	memset(0x8000, 0x00, 0x2000)
	map(room.x*16, room.y*16)
	for de in all(dumb_enemies) do
		remove_tile(de.x-4,de.y-6)
	end
	memcpy(0x8000, 0x6000, 0x2000)
end

function update_world()
	for i=#fallings,1,-1 do
		-- make column fall
		local p=fallings[i]
		fallcolumn(p.x, p.y)
		
		-- update column for next fall
		p.y += 1
		fallings[i]=p
		
		-- column hit ground
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
		for t in all(targets) do
			if did_hit_target(x,y,t) then
				remove_target(t)
			end
		end
	end
	mapset(x, y+1, startcol)
end

function hit_bounds(x,y)
	if (room_state.bounds and (x > 127 or x < 0)) then
		return true
	end
	return false
end

function mapget(x, y)
	if (room_state.bounds and y > 123) return 0b0001111

	if (y < 0 or x < 0 or x > 127 or y > 127) return 0
	local memloc = (0x8000 + ((x + (y << 7)) >> 1))
	if x & 1 == 0 then
		memloc = @memloc	& 0x0f
	else
		memloc = (@memloc & 0xf0) >> 4
	end
	return memloc
end

function mapset(x, y, col)
	if (y < 0 or x < 0 or x > 127 or y > 127) return
	local memloc = (0x8000 + ((x + (y << 7)) >> 1))
	local value = @memloc
	if x % 2 == 0 then
		mask = 0b11110000
		value = (value & mask) | col
	else
		mask = 0b00001111
		value = (value & mask) | (col << 4)
	end
	poke(memloc, value)
end

function clear_line(y, x0, x1)
	--bounds
	if y < 0 or y > 127 then return end
	x0 = mid(0, x0, 127)
	x1 = mid(0, x1, 127)
	
	--order check
	if x1	< x0 then
		local tmp = x0
		x0 = x1
		x1 = tmp
	end

	--memory location
	local row_addr = 0x8000 + (y << 6) --64 bytes per row, 128 pixels

	--start byte
	if (x0 & 1) == 1 then -- x0 is odd, clear right side and increment
		local addr = row_addr + (x0 >> 1) -- >> 1 is the same as / 2
		poke(addr, @addr & 0x0f)
		x0 += 1
	end
	
	--end byte
	if (x1 & 1 == 0) then -- x1 is even, clear left side and decrement
		local addr = row_addr + (x1 >> 1)
		poke(addr, @addr	& 0xf0)
		x1 -= 1
	end

	--middle
	if x0 <= x1	then
		local start_addr = row_addr + (x0 >> 1)
		local bytecount	= (x1 - x0 + 1) >> 1
		memset(start_addr, 0, bytecount)
	end
end

function draw_circ(o_x, o_y, x, y, fall)
	y1 = o_y+y
	y2 = o_y-y
	y3 = o_y+x
	y4 = o_y-x

	clear_line(y1, o_x - x, o_x + x)
	clear_line(y2, o_x - x, o_x + x)
	clear_line(y3, o_x - y, o_x + y)
	clear_line(y4, o_x - y, o_x + y)

	if fall then
		local tmp_fall={}
		if mapget(o_x+x, y2 - 1) != 0 then
			add(tmp_fall, {x=o_x+x, y=y2-1})
		end
		if mapget(o_x+y, y4 - 1) != 0 then
			add(tmp_fall, {x=o_x+y, y=y4-1})
		end
		if mapget(o_x-x, y2 - 1) != 0 then
			add(tmp_fall, {x=o_x-x, y=y2-1})
		end
		if mapget(o_x-y, y4 - 1) != 0 then
			add(tmp_fall, {x=o_x-y, y=y4-1})
		end
		return tmp_fall
	end
end

function explode_brem(o_x, o_y, r, fall)
	local x = 0
	local y = r
	local d = 3 - 2 * r
	-- split logic between falling and not falling
	if fall then
		-- save possible falling pixels
		local tmp_fall=draw_circ(o_x, o_y, x, y, fall)
		local to_add = {}
		while y >= x do
			if d > 0 then
				y -= 1
				d = d + 4 * (x - y) + 10
			else
				d = d + 4 * x + 6
			end
			
			x += 1
			local add_fall=draw_circ(o_x, o_y, x, y, fall)
			for a in all(add_fall) do
				add(tmp_fall, a)
			end
		end

		-- check which pixels to add to fallings
		-- avoids overlapping pixels
		for t in all(tmp_fall) do
			if to_add[tostring(t.x)] == nil then
				to_add[tostring(t.x)] = t
			elseif to_add[tostring(t.x)].y > t.y then
				to_add[tostring(t.x)] = t
			end
		end
		for k,v in pairs(to_add) do
			add(fallings, v)
		end
	end
end
-->8
-- shooting & collisions

-- shot={
--  x,
--  y,
--  xvel,
--  yvel,
--  xprev,
--  yprev,
--  id
-- }
shots = {}
grav = 0.2

modifiers = {
	count = {
		name = "count",
		apply = function(stats, count)
			stats.count += count
			stats.spread += 0.025 * count
		end
	},
	dmg_flat = {
		name = "dmg",
		apply = function(stats, count)
			stats.dmg += 5 * count
		end
	},
	size = {
		name = "size",
		apply = function(stats, count)
			stats.r += 1 * count
		end
	},
	heavy = {
		name = "heavy",
		apply = function(stats, count)
			stats.dmg_mult += 0.5 * count
			stats.force += 2 * count
			stats.pow_mult -= 0.2 * count
			stats.cd += 20 * count
		end
	},
	dmg_mult = {
		name = "dmg_mult",
		apply = function(stats, count)
			stats.dmg_mult += 0.1 * count
		end
	},
	mini = {
		name = "mini",
		apply = function(stats, count)
			stats.dmg_mult *= 0.5^count
			stats.r_mult *= 0.5^count
			stats.cd_mult *= 0.35^count
			stats.force_mult *= 0.5^count
			stats.pow += 0.2 * count
		end
	},
	slow_reload = {
		name = "slow_reload",
		apply = function(stats, count)
			stats.cd_mult += 2.2*count
		end
	}
}

shot_types = {
	basic = {
		name = "basic",
		stats = {
			dmg=25,
			r=6,
			init_r=1,
			force=7,
			count=1,
			spread=0.1,
			pow=1,
			cd=60
		},
		get_child_count = function(s,m)
			return 0
		end,
		init = function(s)
			if s.stats == nil then
				return
			end

			local base_angle = atan2(s.xvel, s.yvel)
			local speed = sqrt(s.xvel*s.xvel + s.yvel*s.yvel)

			local count = max(1, s.stats.count)
			local spread = s.stats.spread

			for i=1,count do
				local vel = pow_spread(base_angle,speed,spread,count,i)
				local xv = vel.x
				local yv = vel.y
				if i == 1 then
					s.xvel = xv
					s.yvel = yv
				else
					local stat_copy = copy_table(s.stats)
					-- avoid changing new shots
					stat_copy.count = 0
					stat_copy.spread = 0
					stat_copy.pow = 1

					shoot_w_stats(s.x, s.y, xv, yv, s.id, s.node, stat_copy)
				end
			end
		end,
		interpret_semantics = function(sem)
			local base = shot_types.basic.stats
			local stats = copy_table(base)
			return apply_sem_basic(sem, stats)
		end,
		update = function(s)
			basic_update(s)
			return true
		end,
		on_collision = function(s, inf)
			explode(s,inf)
		end
	},
	split = {
		name = "split",
		stats = {
			dmg = 10,
			r = 4,
			init_r = 1,
			force = 4,
			count = 3,
			spread = 1.2,
			pow = 1,
			cd = 60
		},
		get_child_count = function(s, mods)
			local sem = resolve_semantic_stats(mods)
			return sem.count + shot_types.split.stats.count
		end,
		init = function(s) end,
		interpret_semantics = function(sem)
			local base = shot_types.split.stats
			local stats = copy_table(base)
			return apply_sem_basic(sem, stats)
		end,
		update = function(s)
			prev_yvel = s.yvel
			basic_update(s)
			if s.yvel > 0 and prev_yvel <= 0 then
				local angle = atan2(s.xvel,s.yvel)
				local speed = sqrt(s.xvel*s.xvel + s.yvel*s.yvel)

				for i=1,s.stats.count do
					local spread = -s.stats.spread * 0.5 + (i - 1) * (s.stats.spread / (s.stats.count - 1))
					if not s.children == nil and count(s.children) >= i then
						shoot(s.x, s.y, angle, speed + spread, s.id, s.children[i])
					else
						shoot(s.x, s.y, angle, speed + spread, s.id, generate_shot(shot_types.basic.name,nil,nil))
					end
				end
				return false
			end
			return true
		end,
		on_collision = function(s, inf)
			explode(s,inf)
		end
	}
}

function pow_spread(base_angle, base_speed, spread, total_count, cur_count)
	local a = base_angle
	local sp = spread

	if total_count > 1 then
		local t = (cur_count - 1) / (total_count - 1)
		sp = -spread * 0.5 + (cur_count - 1) * (spread / (total_count - 1))
	end

	local powspread = 1 + sp
	local xv = cos(a) * base_speed * powspread
	local yv = sin(a) * base_speed * powspread
	return {x=xv,y=yv}
end

function generate_shot(name, modifiers, children)
	return {
		name = name,
		modifiers = modifiers,
		children = children
	}
end

function apply_sem_basic(sem, stats)
	stats.count += sem.count
	stats.dmg += sem.dmg
	stats.dmg *= sem.dmg_mult
	stats.r += sem.r
	stats.r *= sem.r_mult
	stats.init_r = 1
	stats.force += sem.force
	stats.force *= sem.force_mult
	stats.pow += sem.pow
	stats.pow *= sem.pow_mult
	stats.cd += sem.cd
	stats.cd *= sem.cd_mult
	stats.spread += sem.spread
	stats.spread *= sem.spread_mult
	return stats
end

function resolve_semantic_stats(mods)
	local semantics = {
		count=0,
		dmg=0,
		dmg_mult=1,
		r=0,
		r_mult=1,
		force=0,
		force_mult=1,
		pow=0,
		pow_mult=1,
		cd=0,
		cd_mult=1,
		spread = 0,
		spread_mult = 1
	}

	for id,mod in pairs(mods) do
		modifiers[id].apply(semantics, mod.stacks)
	end

	return semantics
end

function resolve_params(shot_node)
	local def = shot_types[shot_node.name]
	local semantics = resolve_semantic_stats(shot_node.modifiers)
	return def.interpret_semantics(semantics)
end

function basic_update(s)
	s.x += s.xvel
	s.y += s.yvel
	s.yvel += grav
end

function explode(s,inf)
	ex_x=flr(inf.x)
	ex_y=flr(inf.y)
	hit_col=mapget(ex_x,ex_y)
	add(exps, {
		x=ex_x,
		y=ex_y,
		r=s.stats.r,
		cur_r=s.stats.init_r,
		hit_ids={},
		dmg=s.stats.dmg,
		force=s.stats.force,
		id=s.id
	})
end

function update_shots()
	for i=#shots,1,-1 do
		s = shots[i]
		s.frame += 1

		-- check collision
		local inf = checkcol(s)
		if inf.didhit then
			if (inf.prevx == inf.x and inf.prevy == inf.y) then
				inf.prevx = s.xprev
				inf.prevy = s.yprev
			end

			local keep_alive = s.on_collision(s, inf)
			if not keep_alive then
				del(shots, s)
				-- map/dirt particles
				n_x,n_y=get_q_normal(ex_x, ex_y)
				p_x=ex_x+n_x*2
				p_y=ex_y+n_y*2
				if inf.hitcol!=0 then
					for k=1,s.stats.force do
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
				return
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

		-- update
		keep_alive = s.update(s)
		if keep_alive == false or s.y > 128 or s.x > 128 or s.x < 0 then
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

function shoot(x, y, angle, vel, id, shot_node)
	local dx = cos(angle)
	local dy = sin(angle)
	local stats = resolve_params(shot_node)
	return shoot_w_stats(x,y,dx*vel,dy*vel,id,shot_node,stats)
end

function shoot_w_stats_angle(x, y, angle, vel, id, shot_node, stats)
	local dx = cos(angle)
	local dy = sin(angle)
	return shoot_w_stats(x,y,dx*vel,dy*vel,id,shot_node,stats)
end

function shoot_w_stats(x, y, xvel, yvel, id, shot_node, stats)
	local s = {
		x = x,
		y = y,
		xvel = xvel*stats.pow,
		yvel = yvel*stats.pow,
		id = id,
		frame = 0,
		stats = stats,
		node = shot_node,
		init = shot_types[shot_node.name].init,
		update = shot_types[shot_node.name].update,
		on_collision = shot_types[shot_node.name].on_collision
	}
	s.init(s)
	add(shots, s)
	return stats.cd
end

function shoot_from_turret(x, y, angle, vel, id, shot_node)
	vel *= 6
	vel += 1
	local dx = cos(angle)
	local dy = sin(angle)
	local ox = x+dx*4
	local oy = y-3+dy*4
	sfx(1)
	return shoot(ox,oy,angle,vel,id,shot_node)
end
-->8
-- explosions
--exp = {
--	x=0,
--	y=0,
--	r=5,
--	cur_r=0,
-- hit_ids=[],
-- dmg=25,
-- id = 1
--}

exps={}

function update_explosions()
	for i=#exps,1,-1 do
		local exp = exps[i]
		local eq = exp.cur_r == exp.r
		explode_brem(exp.x, exp.y, exp.cur_r, eq)
		handle_exp_hits(exp)
	end
end

function handle_exp_hits(exp)
	-- player
	if not tbl_contains(exp.hit_ids,p.id) then
		if handle_exp_hit(exp,p) then
			local dmg=calc_dmg(exp,p.id)
			p.health-=dmg
			if p.health <= 0 then
				gameover = true
			end
			p.is_knocked=true
			local f_x, f_y=get_knockback(p,exp)
			p.xvel=f_x
			p.yvel=f_y
			add(knocked_entities,p)
			add(hit_texts,{
				x=p.x-2,
				y=p.y,
				col=2,
				life=30,
				text=dmg
			})
		end
	end

	-- enemies
	for e in all(dumb_enemies) do
		if not tbl_contains(exp.hit_ids,e.id) then
			if handle_exp_hit(exp,e) then
				local dmg=calc_dmg(exp,e.id)
				e.health-=dmg
				add(hit_texts,{
					x=e.x-2,
					y=e.y,
					col=8,
					life=30,
					text=dmg
				})
				if e.health <= 0 then
					del(dumb_enemies,e)
					check_room_state()
				else
					e.is_knocked=true
					local f_x, f_y=get_knockback(e,exp)
					e.xvel=f_x
					e.yvel=f_y
					add(knocked_entities,e)
				end
			end
		end
	end
	
	for t in all(targets) do
		if not tbl_contains(exp.hit_ids,t.id) then
			local t_hit = circle_rect_col(exp.x,exp.y,exp.cur_r,t.x+3.5,t.y+3.5)
			if t_hit then
				remove_target(t)
			end
		end
	end
end

function calc_dmg(exp, id)
	local dmg=exp.dmg
	if id == exp.id then dmg *= 0.1 end
	dmg=ceil(dmg)
	return dmg
end

function handle_exp_hit(exp,target)
	local hit = cccol(exp.x, exp.y, exp.cur_r, target.x-0.5, target.y - 1, 3)
	if hit.didhit then
		add(exp.hit_ids, target.id)
		return true
	end
	return false
end

function draw_explosions()
	for i=#exps,1,-1 do
		local exp = exps[i]
		circfill(exp.x, exp.y, exp.cur_r, 0)
		circ(exp.x, exp.y, exp.cur_r, 8)
		exp.cur_r += 1
		if exp.cur_r > exp.r then
			del(exps, exp)
		end
	end	
end

function tbl_contains(tbl,item)
	for i=1,#tbl do
		if tbl[i]==item then
			return true
		end
	end
	return false
end
-->8
-- particles

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

	for i=#knocked_entities,1,-1 do
		local ent=knocked_entities[i]
		local inf=checkcol(ent)

		if inf.didhit and inf.hitcol != 100 then
			local hit_x=flr(inf.x)
			local hit_y=flr(inf.y)
			local n_x, n_y=get_q_normal(hit_x,hit_y)
			ent.x=inf.prevx
			ent.y=inf.prevy
			ent.xvel=0
			ent.yvel=0
			ent.is_knocked=false
			ent.is_grounded=true
			del(knocked_entities, ent)
		else
			ent.x += ent.xvel
			ent.y += ent.yvel
			ent.yvel += grav
			if ent.x < 0 and room_state.bounds then
				ent.x -= ent.x
				ent.xvel = -ent.xvel
			elseif ent.x > 127 and room_state.bounds then
				ent.x -= (ent.x-127)
				ent.xvel = -ent.xvel
			end
		end
	end
end

function get_knockback(ent, exp)
	local raw_dir_x=ent.x-exp.x
	local raw_dir_y=ent.y-(exp.y+2) // better sim of center of mass
	local length=sqrt(raw_dir_x^2+raw_dir_y^2)
	local norm_x=raw_dir_x/length
	local norm_y=raw_dir_y/length
	local scaling=1-(exp.cur_r/exp.r)
	local x=norm_x*scaling*exp.force
	local y=norm_y*scaling*exp.force
	return x, -abs(y)
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
-- collisions
function cccol(x0, y0, r0, x1, y1, r1)
	local dx = x1 - x0
	local dy = y1 - y0
	local dist_sq = dx * dx + dy * dy
	local radii_sum = r0 + r1
	local didhit = dist_sq <= radii_sum * radii_sum

	if didhit then
		local dist = sqrt(dist_sq)
		if dist > 0 then
			dx /= dist
			dy /= dist
		end
		return {
			didhit = true,
			x = x0 + dx * r0,
			y = y0 + dy * r0,
			normal_x = dx,
			normal_y = dy
		}
	end
	return {didhit = false}
end

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
 local dx = x1 - x0
 local dy = y1 - y0
 local yi = sgn(dy)
 local xi = sgn(dx)
 local d = (2 * dy * yi) - dx*xi
 local y = y0
 local prevx = x0
 local prevy = y
	local has_bounds = room_state.bounds

 for x=x0,x1,xi do
		local ix = flr(x)
		local iy = flr(y)
		local pixel = 0
		local hit = false

		-- 1. wall bounds
		if has_bounds and (x > 127 or x < 0) then
			hit = true
			pixel = 100
		-- 2. floor bounds
		elseif has_bounds and iy > 123 then
			hit = true
			pixel = 0b0001111
		-- 3. check map
		elseif ix >= 0 and ix < 128 and iy > 0 and iy < 128 then
			-- address: 0x8000 + (x + y * 128) / 2
			-- (iy << 7) is y * 128, >>1 is /2
			local val = @(0x8000 + ((ix + (iy << 7)) >> 1))

			if (ix & 1) == 0 then
				pixel = val & 0x0f
			else
				pixel = (val & 0xf0) >> 4
			end

			if (not(pixel == 0)) then hit = true end
		end

  if hit then
   local d = sqrt(dx*dx+dy*dy)
			if (d > 0) then dx /= d dy /= d end
  	return {
	  	didhit=true,
	  	x=x,
	  	y=y,
	  	prevx=prevx,
	  	prevy=prevy,
				hitcol=pixel
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

	-- check endpoint
	local ix = flr(x1)
	local iy = flr(y1)
	local pixel = 0
	if has_bounds and (x1 > 127 or x1 < 0) then pixel = 100
	elseif has_bounds and iy > 123 then pixel = 15
	elseif ix >= 0 and ix < 128 and iy >= 0 and iy < 128 then
		local val = @(0x8000 + ((ix + (iy<<7)) >> 1))
		if (ix & 1) == 0 then pixel = val & 0x0f
		else pixel = (val & 0xf0) >> 4 end
	end
	if not(pixel == 0) then
		return {
	 	didhit=true,
	 	x=x1,
	 	y=y1,
	 	prevx=prevx,
	 	prevy=prevy,
			hitcol=pixel
	 }
	end
 
	return {didhit=false}
end

function checkcolhigh(x0, y0, x1, y1)
 local dx = x1 - x0
 local dy = y1 - y0
 local xi = sgn(dx)
 local yi = sgn(dy)
 local d = 2 * dx - dy*yi
 local x = x0
 local prevx = x
 local prevy = y0
	local has_bounds = room_state.bounds
 
 for y=y0,y1,yi do
		local ix = flr(x)
		local iy = flr(y)
		local pixel = 0
		local hit = false

		if has_bounds and (x > 127 or x < 0) then
			hit = true
			pixel = 100
		elseif has_bounds and iy > 123 then
			hit = true
			pixel = 15
		elseif ix >= 0 and ix < 128 and iy >= 0 and iy < 128 then
			local val = @(0x8000 + ((ix + (iy<<7)) >> 1))
			if (ix & 1) == 0 then pixel = val & 0x0f
			else pixel = (val & 0xf0) >> 4 end
			if not(pixel == 0) then hit = true end
		end

  if hit then
  	local d = sqrt(dx*dx+dy*dy)
			if d > 0 then dx /= d dy /= d end
  	return {
	  	didhit=true,
	  	x=x,
	  	y=y,
	  	prevx=prevx,
	  	prevy=prevy,
				hitcol=pixel
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

	-- check endpoint
	local ix = flr(x)
	local iy = flr(y)
	local pixel = 0
	local hit = false

	if has_bounds and (x > 127 or x < 0) then pixel = 100
	elseif has_bounds and iy > 123 then pixel = 15
	elseif ix >= 0 and ix < 128 and iy >= 0 and iy < 128 then
		local val = @(0x8000 + ((ix + (iy<<7)) >> 1))
		if (ix & 1) == 0 then pixel = val & 0x0f
		else pixel = (val & 0xf0) >> 4 end
	end

	if not(pixel == 0) then
		return {
			didhit=true,
			x=x1,
			y=y1,
			prevx=prevx,
			prevy=prevy,
			hitcol=pixel
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
-- map types and room loading
room = {x=0,y=0}
room_state={
	completed=false,
	bounds=true
}
targets = {}
dumb_enemies = {}
hit_texts = {}
knocked_entities={}

empty_t = 63
target_t = 62
enemy_t = 61

function load_room(x,y)
 -- clear
	del_table_contents(targets)
	del_table_contents(dumb_enemies)
	del_table_contents(shots)
	del_table_contents(map_parts)
	del_table_contents(parts)
	del_table_contents(exps)

	p.jumps = 3

	-- room wrapping
	if x < 0 then
		x = 15
	elseif x > 15 then
		x = 0
	end
	if y < 0 then
		y = 7
	elseif y > 7 then
		y = 0
	end

	room.x = x
	room.y = y

	for tx=0,15 do
		for ty=0,15 do
		 local tile = mget(room.x*16+tx,room.y*16+ty)
		 if tile==target_t then
		 	add(targets, {
					id=get_entity_id(),
		 		x=tx*8,
		 		y=ty*8
		 	})
			elseif tile==enemy_t then
		 	add(dumb_enemies, {
					id=get_entity_id(),
		 		x=tx*8+4, // offset sprite
		 		y=ty*8+6, // offset sprite
					xvel=0,
					yvel=0,
		 		t_angle=0.4,
					target_angle=0.4,
		 		pow=0.5,
					health=25,
					max_health=25,
					is_knocked=false,
					cd=140,
					cur_cd=140,
					shot = {
						name = shot_types.basic.name,
						modifiers = {
						 slow_reload = { stacks = 1 }
						},
						child = nil
					}
		 	})
			end
		end
	end

	init_world()
	check_room_state() --could load room with no enemies
	fix_player_spawn()
end

function add_modifier(mods, id, stacks)
	local m = mods[id]
	if m then
		m.stacks += stacks
	else
		mods[id] = { stacks = stacks }
	end
end

function del_table_contents(tbl)
	for t in all(tbl) do
		del(tbl, t)
	end
end

function remove_tile(x,y)
	for tx=x,x+7 do
		for ty=y,y+7 do
			pset(tx,ty,0)
		end
	end
end

function remove_target(t)
	del(targets,t)
	add(hit_texts,{
		x=t.x+3,
		y=t.y-3,
		col=10,
		life=30,
		text="+1"
	})
	check_room_state()
end

-- hit_text:x,y,col,life,text
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

function check_room_state()
	if #dumb_enemies==0 and #targets==0 then
		if not room_state.completed then
			p.health += p.health_regen
			p.health = min(p.health, p.max_health)
		end
		room_state.bounds = false
		room_state.completed = true
	else
		room_state.bounds = true
		room_state.completed = false
	end
end

function fix_player_spawn()
	if mapget(flr(p.x),flr(p.y)) == 0 then
		return
	end
	p.y = get_ground(flr(p.x),flr(p.y))-1
	-- could be that no ground was found, so check again from the bottom
	if mapget(flr(p.x),flr(p.y)) != 0 then
		p.y = get_ground(flr(p.x),127)-1
	end
end

function draw_bounds()
	if room_state.bounds then
		line(127,0,127,124,1)
		line(127,124,0,124,1)
		line(0,124,0,0,1)
	end
end

function did_hit_target(x,y,t)
	if x>=t.x and x<=t.x+7 and y>=t.y	and y<=t.y+7 then
		return true
	end
	return false
end

function circle_rect_col(cx,cy,r,rx,ry)
	local testx = cx
	local testy = cy
	local w = 3.5

	if cx < rx then testx=rx
	elseif cx > rx+w then testx=rx+w
	end
	if cy < ry then testy = ry
	elseif cy > ry+w then testy=ry+w
	end

	local distx=cx-testx
	local disty=cy-testy
	local dist=sqrt(distx^2+disty^2)

	if dist <= r then
		return true
	end
	return false
end

-->8
-- enemies
function update_enemies()
	for de in all(dumb_enemies) do
		if de.cur_cd<=0 then
			de.pow=abs(p.x-de.x)*0.65*0.01
			local cd = shoot_from_turret(de.x,de.y,de.t_angle,de.pow,de.id,de.shot)
			de.cur_cd = cd
			de.cd = cd
		end
		de.cur_cd-=1
		
		if mapget(de.x,de.y+1)==0 then
			de.y+=1
		end

		// mirror based on player pos
		if p.x-de.x<0 and de.target_angle>0.5 then
			de.target_angle-=(de.target_angle-0.5)*2
		elseif p.x-de.x>0 and de.target_angle<0.5 then
			de.target_angle+=(0.5-de.target_angle)*2
		end

		de.t_angle=de.target_angle+sin(time()*0.2312)*0.08
	end
end

function draw_enemies()
	for de in all(dumb_enemies) do
		de.x -= 4
		de.y -= 6
		spr(61,de.x,de.y)
		de.x += 4
		de.y += 6

		local tur_x = cos(de.t_angle)*4
		local tur_y = sin(de.t_angle)*4
		local t_end_x=de.x+tur_x
		local t_end_y=de.y-3+tur_y
		line(de.x, de.y-3, t_end_x, t_end_y, 7)
	end
end
-->8
-- todo
-- currently working on: ensure that map particles spawn in the normal direction to avoid falling issues

--performance:
---could consider batch removal of
---dirt in explosions, e.g.
----1. loop over all explosions,
----get pixels for removal back
----2. batch remove all pixels

--bugs:
--terrain still seems to flip if
---multiple shots hit the same area

--improvements:
--how do we ensure the correct
---number of children when generating
---the shot? right now nothing stops
---us from supplying more children
---than a shot can take...
--implement more shot types and
---modifiers

--pickups:
--add shots and modifiers in
---levels so you can pick them up
--add picking up method
--ui showing what pickups you
---have. keep them in order of
---pickup time for now

--test:
---test all modifiers

-->8
--ui
function draw_ui()
 for e in all(dumb_enemies) do
		draw_bar(e.cur_cd,e.cd,e.x,e.y-6,10)
		draw_bar(e.health,e.max_health,e.x,e.y-7,8)
 end

	if gameover then
		print("game over", 42, 60, 8)
		print("press ❎ to restart", 24, 70, 8)
		return
	end
	draw_bar(p.cur_cd,p.cd,p.x,p.y-6,10)
	draw_bar(p.health,p.max_health,p.x,p.y-7,8)
	print("jumps:"..tostr(p.jumps), 0, 123, 7)
end

function draw_bar(cur_t,max_t,x,y,col)
	local length=7
	local draw_length=(cur_t/max_t)*length
	if draw_length > 0 then
		line(x-4,y,x-4+draw_length,y,col)
	end
end

--utils
function copy_table(t)
	local n = {}
	for k,v in pairs(t) do n[k] = v end
	return n
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000880008787787800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000088888808787787800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000088888808788887800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000151515158777777800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000515151518888888800000000
__map__
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000003f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
020202020202020202020202023f3f3f3f000000000000000000003e0000000000000000000000000000003e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
131313131313131313131313133e3f3f3f00000000000000000000033f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000033f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000000300000000000000000000000000000000000000000000000000000000090800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3d3f3f3f00000000000000000000003f00000000000000000000000000000000000000000000000000000009131308000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0202020202020202020202020202020213131308000000000000003f0000000000003e0000000000000000131313000000000000003f0913131313080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
13131313131313131313131313131313131313131313131313131313131313131313130000000000000000133e131313130000003f0913131313131308003d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0313131303030303131303030313030313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000000013130000000000000000000000000000131300000000000000003f3f3f3f00001313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000003f3f3f3f000000000000003f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
003d0000000000000000000000003d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0013000000000000000000000000130000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
003f0000000000000000000000003f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1313131313131313131313131313131300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100000012002120041200512007120091200b1200c1200e1201012011120131201512017120181201a1201c1201d1201f1202112023120241202612028120291202b1202d1202f12030120321203412035120
570100000015003150061500915013150291501d150171501415011150101500c1500a15008150071500515002150001500010000100021000010000100011000010000100001000110000100001000010000100
8e1000003961300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
d61000003f62000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
8e20000018a5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
