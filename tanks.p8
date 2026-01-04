pico-8 cartridge // http://www.pico-8.com
version 43
__lua__
-- general

function _init()
	entity_id = 0
	gameover = false
	wait_t = 0
	t = 0
	modifier_names = {}
	shot_names = {}
	for k,v in pairs(modifiers) do
		add(modifier_names, k)
	end
	for k,v	in pairs(shot_types) do
		add(shot_names, k)
	end
	p={
		id = get_entity_id(),
		x = 64,
		y = get_ground(64,64),
		xvel=0, -- used when knocked
		yvel=0,
		move = false,
		spr = 0,
		t_angle = 0.125,
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
			spr = shot_types.basic.spr,
			modifiers = {
			},
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
	update_pickups()
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

--https://www.lexaloffle.com/bbs/?tid=36059
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
		if btn(⬅️) and (room_state.bounds==false or p.x > 0) then
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
			if (p.jumps > 0 and not p.is_knocked) then
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
	if btn(🅾️) and p.cur_cd <= 0 then --z
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

	-- check collision and interactions with pickups
	if btnp(🅾️,1) then
		drop_pickup()
	end

	for pick in all(pickups) do
		if tile_aabb(p.x-4, p.y-6, pick.x, pick.y) then
			p.pickup_hover = pick
			if btnp(⬅️,1) then
				apply_pickup(pick)
				remove_pickup(pick)
			elseif btnp(⬆️,1) then
				swap_pickup(pick)
			end
			return
		end
	end
	p.pickup_hover = nil
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
	spr(p.spr, p.x-4, p.y-6)

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
	for p in all(pickups) do
		remove_tile(p.x,p.y)
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

function explode_brem(o_x, o_y, r, fall)
	local x = 0
	local y = r
	local d = 3 - 2 * r
	if fall then
		-- save possible falling pixels
		local tmp_fall=draw_circ(o_x, o_y, x, y)
		local to_add = {}
		while y >= x do
			if d > 0 then
				y -= 1
				d = d + 4 * (x - y) + 10
			else
				d = d + 4 * x + 6
			end
			
			x += 1
			local add_fall=draw_circ(o_x, o_y, x, y)
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
		spr = 35,
		apply = function(stats, count)
			stats.count += count
			stats.spread += 0.025 * count
		end
	},
	dmg_flat = {
		name = "dmg_flat",
		spr = 36,
		apply = function(stats, count)
			stats.dmg += 5 * count
		end
	},
	size = {
		name = "size",
		spr = 37,
		apply = function(stats, count)
			stats.r += 3 * count
		end
	},
	heavy = {
		name = "heavy",
		spr = 38,
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
		spr = 39,
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
		spr = 33,
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
		init = function(s)
			single_shot_init(s)
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
		spr = 34,
		stats = {
			dmg = 10,
			r = 4,
			init_r = 1,
			force = 4,
			count = 3,
			spread = 1.2,
			pow = 1,
			cd = 75
		},
		init = function(s) end,
		interpret_semantics = function(sem)
			local base = shot_types.split.stats
			local stats = copy_table(base)
			return apply_sem_basic(sem, stats)
		end,
		finalize_stats	= function(node, stats)
			local child = node.child
			if child == nil then
				child = generate_shot(shot_types.basic.name,nil,nil)
			end
			local child_stats = resolve_params(child)
			local child_cd = child_stats.cd

			local diff = child_cd - 60
			stats.cd += diff

			return stats
		end,
		update = function(s)
		 local prev_yvel = s.yvel
			basic_update(s)
			if prev_yvel <= 0 and s.yvel > 0 then
				local angle = atan2(s.xvel,s.yvel)
				local speed = sqrt(s.xvel*s.xvel + s.yvel*s.yvel)

				local child_node = s.node.child
				if child_node == nil then
					child_node = generate_shot(shot_types.basic.name,nil,nil)
				end

				local child_base_stats = resolve_params(child_node)
				local multiplier_stats = apply_multiplier_stats(shot_types.split.stats, s.stats, child_base_stats)
				child_base_stats.dmg *= 0.5
				child_base_stats.force *= 0.5
				child_base_stats.r *= 0.8

				for i=1,s.stats.count do
					local child_stats	= copy_table(child_base_stats)
					local spread = -s.stats.spread * 0.5 + (i - 1) * (s.stats.spread / (s.stats.count - 1))
					local child_speed = speed + spread
					local xv = cos(angle) * child_speed
					local yv = sin(angle) * child_speed

					shoot_w_stats(s.x, s.y, xv, yv, s.id, child_node, child_stats)
				end
				return false
			end
			return true
		end,
		on_collision = function(s, inf)
			explode(s,inf)
		end
	},
	pixel = {
		name = "pixel",
		spr = 50,
		stats = {
			dmg = 10,
			r = 4,
			init_r = 1,
			force = 4,
			count = 3,
			spread = 0.6,
			pow = 1,
			cd = 75
		},
		init = function(s) end,
		interpret_semantics = function(sem)
			local base = shot_types.pixel.stats
			local stats = copy_table(base)
			return apply_sem_basic(sem, stats)
		end,
		update = function(s)
			basic_update(s)
			local child_node = s.node.child
			if child_node == nil	then
				child_node = generate_shot(shot_types.basic.name,nil,nil)
			end
			local child_stats = resolve_params(child_node)

			if s.frame % flr(child_stats.cd / 20) == 0 and s.frame >= 10 then
				local multiplier_stats = apply_multiplier_stats(shot_types.pixel.stats, s.stats, child_stats)
				child_stats.dmg *= 0.2
				child_stats.force *= 0.2
				child_stats.r *= 0.4

				local angle = rnd()
				local xv = cos(angle) * s.stats.spread
				local yv	= sin(angle) * s.stats.spread

				shoot_w_stats(s.x, s.y, xv, yv, s.id, child_node, child_stats)
			end

			return true
		end,
		on_collision = function(s, inf)
			explode(s,inf)
		end
	},
	bounce = {
		name = "bounce",
		spr = 49,
		stats = {
			dmg=20,
			r=5,
			init_r=1,
			force=6,
			count=2,
			spread=0.1,
			pow=1,
			cd=65
		},
		init = function(s) end,
		interpret_semantics = function(sem)
			local base = shot_types.bounce.stats
			local stats = copy_table(base)
			return apply_sem_basic(sem, stats)
		end,
		update = function(s)
			basic_update(s)
			return true
		end,
		on_collision = function(s, inf)
			explode(s,inf)

			-- create another bouncing shot
			s.stats.count -= 1
			local nx, ny = get_normal(inf.x, inf.y)

			-- mirror around normal
			local xvel = s.xvel - 2 * (s.xvel * nx + s.yvel * ny) * nx
			local yvel = s.yvel - 2 * (s.xvel * nx + s.yvel * ny) * ny
			local newx = s.x+nx
			local newy = s.y+ny
			newx = mid(0, newx, 127)
			newy = mid(0, newy, 127)

			if s.stats.count <= 0 then
				if s.node.child then
					local child_stats = resolve_params(s.node.child)
					local multiplier_stats = apply_multiplier_stats(shot_types.bounce.stats, s.stats, child_stats)
					shoot_w_stats(newx, newy, xvel, yvel, s.id, s.node.child, child_stats)
				end
			else
				shoot_w_stats(newx, newy, xvel, yvel, s.id, s.node, s.stats)
			end
		end
	},
	sniper = {
		name = "sniper",
		spr = 48,
		stats = {
			dmg=65,
			r=1,
			init_r=1,
			force=5,
			count=1,
			spread=0.0,
			pow=1,
			cd=80
		},
		init = function(s)
			single_shot_init(s)
		end,
		interpret_semantics = function(sem)
			local base = shot_types.sniper.stats
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
	grndsplit = {
		name = "grndsplit",
		spr = 51,
		stats = {
			dmg=10,
			r=4,
			init_r=1,
			force=4,
			count=3,
			spread=1.2,
			pow=1,
			cd=75
		},
		init = function(s) end,
		interpret_semantics = function(sem)
			local base = shot_types.grndsplit.stats
			local stats = copy_table(base)
			return apply_sem_basic(sem, stats)
		end,
		update = function(s)
			basic_update(s)
			return true
		end,
		on_collision = function(s, inf)
			explode(s,inf)

			local angle = 0.25 -- straight up
			local speed = 3.25
			local child_node = s.node.child
			if child_node == nil then
				child_node = generate_shot(shot_types.basic.name,nil,nil)
			end

			local child_stats = resolve_params(child_node)
			local multiplier_stats = apply_multiplier_stats(shot_types.split.stats, s.stats, child_stats)
			child_stats.dmg *= 0.5
			child_stats.force *= 0.5
			child_stats.r *= 0.8

			for i=1,s.stats.count do
				local spread = -s.stats.spread * 0.5 + (i - 1) * (s.stats.spread / (s.stats.count - 1))
				local xv = cos(angle + spread * 0.03) * speed
				local yv = sin(angle + spread * 0.03) * speed

				shoot_w_stats(inf.x, inf.y - 1, xv, yv, s.id, child_node, child_stats)
			end
		end
	},
	laser = {
		name = "laser",
		spr = 52,
		stats = {
			dmg=40,
			r=3,
			init_r=1,
			force=2,
			count=1,
			spread=0,
			pow=3,
			cd=45
		},
		init = function(s)
			single_shot_init(s)
		end,
		interpret_semantics = function(sem)
			local base = shot_types.laser.stats
			local stats = copy_table(base)
			return apply_sem_basic(sem, stats)
		end,
		update = function(s)
			s.x += s.xvel
			s.y += s.yvel
			return true
		end,
		on_collision = function(s, inf)
			explode(s,inf)
		end
	},
}

function single_shot_init(shot)
	if shot.stats == nil then
		return
	end

	local base_angle = atan2(shot.xvel, shot.yvel)
	local speed = sqrt(shot.xvel*shot.xvel + shot.yvel*shot.yvel)

	local count = max(1, shot.stats.count)
	local spread = shot.stats.spread

	for i=1,count do
		local vel = pow_spread(base_angle,speed,spread,count,i)
		local xv = vel.x
		local yv = vel.y
		if i == 1 then
			shot.xvel = xv
			shot.yvel = yv
		else
			local stat_copy = copy_table(shot.stats)
			-- avoid changing new shots
			stat_copy.count = 0
			stat_copy.spread = 0
			stat_copy.pow = 1

			shoot_w_stats(shot.x, shot.y, xv, yv, shot.id, shot.node, stat_copy)
		end
	end
end

function apply_multiplier_stats(base_stats, stats, child_stats)
	child_stats.dmg *= stats.dmg / base_stats.dmg
	child_stats.r *= stats.r / base_stats.r
	child_stats.force *= stats.force / base_stats.force
end

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

function generate_shot(name, modifiers, child)
	return {
		name = name,
		modifiers = modifiers,
		child = child
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
	local stats = def.interpret_semantics(semantics)
	if def.finalize_stats then
		stats = def.finalize_stats(shot_node, stats)
	end
	return stats
end

function basic_update(s)
	s.x += s.xvel
	s.y += s.yvel
	s.yvel += grav
end

function explode(s,inf)
	local ex_x=flr(inf.x)
	local ex_y=flr(inf.y)

	if inf.y < 0 then
		del(shots,s) return
	end
	del(shots, s)
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

	-- map/dirt particles
	hit_col=mapget(ex_x,ex_y)
	if inf.hitcol !=0 then
		local n_x,n_y=get_q_normal(ex_x, ex_y)
		local p_x=ex_x+n_x
		local p_y=ex_y+n_y
		local base_angle=atan2(n_x,n_y)

		for k=1,(s.stats.r-1)*1.35 do
			local ang = base_angle + rnd(0.5)-0.25
			local vel = rnd(s.stats.r * 0.25)+s.stats.r * 0.15
			add(map_parts, {
				x=flr(p_x),
				y=flr(p_y),
				xvel=cos(ang)*vel,
				yvel=sin(ang)*vel,
				xprev=flr(inf.prevx),
				yprev=flr(inf.prevy),
				life=120,
				col=inf.hitcol
			})
		end
	end
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

			local keep_alive = shot_types[s.node.name].on_collision(s, inf)
			if not keep_alive then
				return
			end
		end

		-- particles
		add(parts, {
			x=s.x,
			y=s.y,
			xvel=rnd(1.5) - 0.75,
			yvel=rnd(1.5) - 0.75,
			xprev=s.x,
			yprev=s.y,
			life=rnd(10)+5,
			col=8+rnd(2)+1
		})

		-- update
		keep_alive = shot_types[s.node.name].update(s)
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
		line(s.x,s.y,s.x-s.xvel,s.y-s.yvel,7)
		pset(s.x+dx,s.y+dy,8)
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
	}
	shot_types[shot_node.name].init(s)
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

function shoot_from_turret_raw(x, y, xv, yv, id, shot_node)
	local stats = resolve_params(shot_node)
	local s = {
		x = x,
		y = y,
		xvel = xv*stats.pow,
		yvel = yv*stats.pow,
		id = id,
		frame = 0,
		stats = stats,
		node = shot_node,
	}
	shot_types[shot_node.name].init(s)
	add(shots, s)
	sfx(1)
	return stats.cd
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
		local eq = exp.cur_r >= exp.r
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
				end
				e.is_knocked=true
				local f_x, f_y=get_knockback(e,exp)
				e.xvel=f_x
				e.yvel=f_y
				add(knocked_entities,e)
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
		if exp.cur_r >= exp.r then
			del(exps, exp)
		end
		circfill(exp.x, exp.y, exp.cur_r, 0)
		circ(exp.x, exp.y, exp.cur_r, 8)
		exp.cur_r += 1
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

--particle = {
--	x,
--	y,
--	xvel,
--	yvel,
-- xprev,
-- yprev,
--	life,
--	col
--

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
			local hit_x=flr(inf.x)
			local hit_y=flr(inf.y)
			local n_x,n_y=get_q_normal(hit_x,hit_y)
			local mp_x=hit_x+n_x
			local mp_y=hit_y+n_y
			mapset(mp_x, mp_y, mp.col)
			if mapget(mp_x, mp_y + 1) == 0 then
				add(fallings, {x=mp_x, y=mp_y})
			end
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

		if inf.didhit and not inf.oob then
			local hit_x=flr(inf.x)
			local hit_y=flr(inf.y)
			if (hit_x == flr(inf.prevx) and hit_y == flr(inf.prevy)) then
				local n_x, n_y=get_normal(hit_x,hit_y)
				inf.prevx = hit_x + n_x
				inf.prevy = hit_y + n_y
			end
			ent.x=inf.prevx
			ent.y=inf.prevy
			ent.xvel=0
			ent.yvel=0
			ent.is_knocked=false
			ent.is_grounded=mapget(flr(ent.x),flr(ent.y+1)) == 0
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
	local scaling=1-((exp.cur_r+0.1)/exp.r)
	if exp.r == 1	then scaling = 1 end
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
		local oob = false

		-- 1. wall bounds
		if has_bounds and (x >= 128 or x < 0) then
			hit = true
			pixel = 15
			oob = true
		-- 2. floor bounds
		elseif has_bounds and iy > 123 then
			hit = true
			pixel = 15
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
				oob=oob,
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
	local oob = false

	if has_bounds and (x1 >= 128 or x1 < 0) then
		pixel = 15
		oob = true
	elseif has_bounds and iy > 123 then pixel = 15
	elseif ix >= 0 and ix < 128 and iy >= 0 and iy < 128 then
		local val = @(0x8000 + ((ix + (iy<<7)) >> 1))
		if (ix & 1) == 0 then pixel = val & 0x0f
		else pixel = (val & 0xf0) >> 4 end
	end
	if not(pixel == 0) then
		return {
	 	didhit=true,
			oob = oob,
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
		local oob = false
		local hit = false

		if has_bounds and (x >= 128 or x < 0) then
			hit = true
			pixel = 15
			oob = true
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
				oob = oob,
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
	local oob = false

	if has_bounds and (x >= 128 or x < 0) then
		pixel = 15
		oob = true
	elseif has_bounds and iy > 123 then pixel = 15
	elseif ix >= 0 and ix < 128 and iy >= 0 and iy < 128 then
		local val = @(0x8000 + ((ix + (iy<<7)) >> 1))
		if (ix & 1) == 0 then pixel = val & 0x0f
		else pixel = (val & 0xf0) >> 4 end
	end

	if not(pixel == 0) then
		return {
			didhit=true,
			oob = oob,
			x=x1,
			y=y1,
			prevx=prevx,
			prevy=prevy,
			hitcol=pixel
		}
	end

	return {didhit=false}
end

function tile_aabb(x1, y1, x2, y2)
	return x1 < x2 + 8 and
		x2 < x1 + 8 and
		y1 < y2 + 8 and
		y2 < y1 + 8
end

-- get the terrain normal at (x, y)
function get_normal(x, y)
 local dx = 0
 local dy = 0

 for offset_y = -1, 1 do
  for offset_x = -1, 1 do
   if offset_x != 0 or offset_y != 0 then

				local xo = flr(x + offset_x)
				local	yo = flr(y + offset_y)
    local value = mapget(flr(x + offset_x), flr(y + offset_y))
				if xo < 0 or xo >= 128 or yo < 0 or yo >= 128 then
					value = 1
				end
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
	if x > 127 then return -1, 0 end
	if x < 0 then return 1, 0 end
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
pickups = {}
hit_texts = {}
knocked_entities={}

empty_t = 63
target_t = 62
enemy_t = 61
pickup_t = 32

function load_room(x,y)
 -- clear
	del_table_contents(targets)
	del_table_contents(dumb_enemies)
	del_table_contents(pickups)
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
		 		x=tx*8+4, -- offset sprite
		 		y=ty*8+6, -- offset sprite
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
			elseif tile==pickup_t then
				spawn_pickup(tx*8,ty*8)
			end
		end
	end

	init_world()
	check_room_state() --could load room with no enemies
	fix_player_spawn()
end

function spawn_pickup(x,y)
	local is_modifier	= (rnd(1) < 0.6)
	local type = is_modifier and "modifier" or "shot"
	local spr, name
	if is_modifier then
		while spr == nil do
			local m = modifiers[rnd(modifier_names)]
			spr = m.spr
			name = m.name
		end
	else
		while spr == nil or name == "basic" do
			local s = shot_types[rnd(shot_names)]
			spr = s.spr
			name = s.name
		end
	end

	add(pickups, {
		x=x,
		y=y,
		type=type,
		spr=spr,
		name=name,
	})
end

function drop_pickup()
	local parent = nil
	local leaf = p.shot

	while leaf.child != nil do
		parent = leaf
		leaf = leaf.child
	end

	local mod_name, mod_data = next(leaf.modifiers)

	if mod_name then
		-- Spawn the modifier pickup
		add(pickups, {
			x = p.x - 4,
			y = p.y - 6,
			spr = modifiers[mod_name].spr,
			type = "modifier",
			name = mod_name
		})

		-- Remove from player inventory
		if mod_data.stacks > 1 then
			mod_data.stacks -= 1
		else
			leaf.modifiers[mod_name] = nil
		end
		
		return
	end

	if parent then
		add(pickups, {
			x = p.x - 4,
			y = p.y - 6,
			spr = shot_types[leaf.name].spr,
			type = "shot",
			name = leaf.name
		})

		-- Cut the link
		parent.child = nil
	end
end

function apply_pickup(pickup)
	-- find leaf of shot tree
	local leaf = p.shot
	while leaf.child != nil do
		leaf = leaf.child
	end

	-- pick up the pickup
	if pickup.type	== "modifier" then
		local existing = leaf.modifiers[pickup.name]
		if existing	then
			existing.stacks += 1
		else
			leaf.modifiers[pickup.name] = { stacks = 1 }
		end
	else
		local new_shot = {
			name = pickup.name,
			spr = pickup.spr,
			modifiers = {}
		}
		leaf.child = new_shot
	end
end

function swap_pickup(pickup)
	if pickup.type == "modifier" then
		return -- can only swap shots
	end

	-- find leaf of shot tree
	local parent = nil
	local leaf = p.shot
	while leaf.child != nil do
		parent = leaf
		leaf = leaf.child
	end

	-- drop current pickup
	add(pickups, {
		x = p.x - 4,
		y = p.y - 6,
		spr = shot_types[leaf.name].spr,
		type = "shot",
		name = leaf.name
	})

	local new_shot = {
		name = pickup.name,
		spr = pickup.spr,
		modifiers = leaf.modifiers
	}
	remove_pickup(pickup)
	if parent	then
		parent.child = new_shot
	else
		p.shot = new_shot
	end

end

function remove_pickup(pickup)
	del(pickups, pickup)
end

function add_modifier(mods, id, stacks)
	local m = mods[id]
	if m then
		m.stacks += stacks
	else
		mods[id] = { stacks = stacks }
	end
end

function update_pickups()
	for	p in all(pickups) do
		local grnd = get_ground(flr(p.x+4),flr(p.y+7))
		p.y = grnd-7
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

	for p in all(pickups) do
		local draw_y = sin(time()*0.4)*3
		spr(p.spr, p.x, draw_y + p.y)
		add(parts, {
			x=p.x + 4 + rnd(1.5) - 0.75,
			y=p.y + 4 + draw_y + rnd(1.5) - 0.75,
			xvel=rnd(1.5) - 0.75,
			yvel=rnd(1.5) - 0.75,
			xprev=p.x,
			yprev=p.y + draw_y,
			life=10,
			col=12
		})
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
		local barrel_x = de.x + cos(de.t_angle) * 4
		local barrel_y = (de.y - 3) + sin(de.t_angle) * 4
		local a,p,xv,yv = get_ballistics(barrel_x, barrel_y, p.x+4, p.y+6, 1.5)
		
		de.target_angle = a
		de.pow = p 

		local a_diff = de.target_angle - de.t_angle
		if a_diff > 0.5 then  a_diff -= 1 end
		if a_diff < -0.5 then a_diff += 1 end
		de.t_angle += a_diff * 0.1

		if de.cur_cd<=0 then
			local a_spread = rnd(0.05) - 0.025
			local p_spread = 0.95 + rnd(0.1)
			xv = (xv * cos(a_spread) - yv * sin(a_spread)) * p_spread
			yv = (xv * sin(a_spread) + yv * cos(a_spread)) * p_spread

			local cd = shoot_from_turret_raw(barrel_x,barrel_y,xv,yv,de.id,de.shot)
			de.cur_cd = cd
			de.cd = cd
		end
		de.cur_cd-=1
		
		if mapget(flr(de.x),flr(de.y+1))==0 then
			de.y+=1
		end
	end
end

function get_ballistics(ox, oy, tx, ty, speed_coef)
	local dx = tx - ox
	local dy = ty - oy
	local t = ceil(max(abs(dx) / speed_coef, 15))
	local vx = dx/t
	local vy = (dy - (grav * t * (t - 1) / 2)) / t
	local angle = atan2(vx, vy)
	local power = sqrt(vx * vx + vy * vy)
	return angle,power,vx,vy
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
-- currently working on: save cleared rooms

-- shots:
-- heavy shot
-- fireworks

--performance:
---could consider batch removal of
---dirt in explosions, e.g.
----1. loop over all explosions,
----get pixels for removal back
----2. batch remove all pixels

--tanks can get stuck in ceiling when flying upwards
--terrain still seems to flip if
---multiple shots hit the same area

--pickups:
--save pickups and cleared rooms
---so we don't have to clear
---rooms again, and can't pickup
---the same powerup multiple times

-->8
--ui
function draw_ui()
 for e in all(dumb_enemies) do
		draw_bar(e.cur_cd,e.cd,e.x,e.y-6,10)
		draw_bar(e.health,e.max_health,e.x,e.y-7,8)
 end

	if gameover then
		print("game over", 42, 60, 8)
		print("press 🅾️ to restart", 24, 70, 8)
		return
	end
	draw_bar(p.cur_cd,p.cd,p.x,p.y-6,10)
	draw_bar(p.health,p.max_health,p.x,p.y-7,8)
	print("jumps:"..tostr(p.jumps), 0, 123, 7)

	-- pickups
	local shot = p.shot
	local drawx = 0
	while not (shot == nil) do
		spr(shot.spr, drawx, 0)
		drawx += 8

		for id,mod in pairs(shot.modifiers) do
			spr(modifiers[id].spr, drawx, 0)
			if mod.stacks > 1 then
				print(tostr(mod.stacks), drawx+2, 8, 7)
			end
			drawx += 8
		end

		shot = shot.child
	end

	if not(p.pickup_hover == nil) then
		if p.pickup_hover.type == "modifier" then
			print("press s to pick up "..p.pickup_hover.name, 8, 32, 7)
		else
			print("press s to pick up "..p.pickup_hover.name, 8, 32, 7)
			print("press e to swap", 16, 40, 7)
		end
	end
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
11111111111111111111111133333333333333333333333333333333333333330000000000000000000000000000000000000000000000000000000000000000
10000001100000011000000130000703308080033008800330000003305550030000000000000000000000000000000000000000000000000000000000000000
10000001100000011000000130007703308080033080080335555553305000030000000000000000000000000000000000000000000000000000000000000000
1000000110099001199999a1307007033080800338070083355555033555d6630000000000000000000000000000000000000000000000000000000000000000
10000001100990011009900137770703380008833877708330055003355fd5530000000000000000000000000000000000000000000000000000000000000000
100000011000000110090a01307007033008000330870803300550033555d6630000000000000000000000000000000000000000000000000000000000000000
1000000110000001100a000130007773308088033008800335555553355fd5530000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111133333333333333333333333333333333333333330000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000008888888800000000
15000051100000011000000110000001100000010000000000000000000000000000000000000000000000000000000000000000000000008777777800000000
10500501190000a11000000110000001100000010000000000000000000000000000000000000000000000000000000000000000000000008788887800000000
1008800110900901199999a11a0a0a01100009010000000000000000000000000000000000000000000000000000000000000000000880008787787800000000
10088001100909011909090110999001188888a10000000000000000000000000000000000000000000000000000000000000000088888808787787800000000
105005011009900119090a0110090001100009010000000000000000000000000000000000000000000000000000000000000000088888808788887800000000
15000051100090011a0a000110090001100000010000000000000000000000000000000000000000000000000000000000000000151515158777777800000000
11111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000515151518888888800000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000f3f3f3f3f3f300000000000000000000f3f3f3f3f30000000000000000000000000000000000000000
__map__
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000003f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
020202020202020202020202023f3f3f3f000000000000000000003e0000000000000000000000000000003e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
131313131313131313131313133e3f3f3f00000000000000000000033f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000033f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000907060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f000000000000000000000003000000000000000000000000000000000000000000000000000000000020000000000000000000000913201307063d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f0000000000000000000000030000000000000000000000000000000000000000000000000000000009080000000000000000000913131313131307063f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f203f3f3f3f3f00000000000000000000003f00000000000000000000000000000000000000000000000000000009131308000000000000000913131313131313131307063d00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0202020202020202020202020202020213131308000000000000003f2000000000003e0000000000000000131313000000000000003f09131313130800000000000913131313131313131313131307060000000000000000000000000000003d0000000000000000000000000000000000000000000000000000000000000000
13131313131313131313131313131313131313131313131313131313131313131313130000000000000000133e131313130000003f0913131313131308003d000913131313131313131313131313131305000000000000003d000000000009020000000000000000000000000000000000000000000000000000000000000000
13131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313130400000000000902020706003d0913130000000000000000000000000000000000000000000000000000000000000000
1313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313130500000009131313131302021313130000000000000000000000000000000000000000000000000000000000000000
0313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313130400200913131313131313131313130000000000000000000000000000000000000000000000000000000000000000
0313131303030303131303030313030313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313130000000000000000000000000000000000000000000000000000000000000000
3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f00000000000000000000003f3f3f000000000000003d0000000000003f3f00000000000000003f3f3f3f00003f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000003f3f3f3f000000000000003f00000000000000000000003d0913020202020202020202020202020202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000091313000000000000000000000000003f3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000009131313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
003d0000000000000000000000003d00000000000000000000003d0913131313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0013000000000000000000000000130000000000000000000000091313131313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000009131313131313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000091313131313131300000000003d0000203f3d00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
003f0000000000000020000000003f0000000000000000091313131313131313020202020202020202020202020202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
131313131313131313131313131313130000000000000913131313131313131300000000000000000000000000003f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000091313131320131313131300000000000000000000000000003f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000091313131313131313131313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000091313131313131313131313130000003d003f000000000000003d00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000913131313131313131313131313030303030303030303030303030303030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000091313131313131313131313131303030303030303030303030303030303030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000009131313131313131313131313131313030303030303030303030303030303030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
