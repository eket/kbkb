import Keyboard
import Char
import List
import Window
import String

-- UTIL

type V = {x:Float, y:Float}

--v : (x:Float, y:Float) -> V
v (x, y) = {x = x, y = y}
v2p {x, y} = (x, y)
vabs {x, y} = sqrt <| (x ^ 2) + (y ^ 2)
vadd v1 v2 = {x = v1.x + v2.x, y = v1.y + v2.y}
vsub v1 v2 = {x = v1.x - v2.x, y = v1.y - v2.y}
vmulf {x, y} f = {x = x * f, y = y * f}


-- MODEL


p = {pos=v(0,0), vel=v(0,0), acc=v(0,0), target=32}

unit = 100
accel = 0.002 * unit
max_vel = 0.02 * unit

make_row n dx x y =
  if n > 1 then v(x, y) :: make_row (n-1) dx (x+dx) y else [v(x, y)] 

grid = map (\(n, y) -> make_row n unit (0 - unit * (n/2)) (unit*y))[
  (12, 2), 
  (11, 1), 
  (10, 0),
  (9, -1)]

max_x = (last <| head grid).x
min_x = (head <| head grid).x
max_y = (last <| head grid).y
min_y = (last <| last grid).y

key_rows = map String.toList [
   "1234567890-="
  ,"QWERTYUIOP["
  ,"ASDFGHJKL;"
  ,"ZXCVBNM,. " ]


-- UPDATE -- ("m" is for Mario)

key_found k = 
  any (\c' -> k == Char.toCode c') (concat key_rows)

key_to_coord k' = 
  let ckr = concat key_rows
      len = List.length ckr
      fn n ks = case ks of 
        (k::ks') -> if k' == Char.toCode k then n else fn (n-1) ks'
        _        -> 0
  in head <| drop (len - fn len ckr) (concat grid)

--jump t m = 
--  if m.target == 32 then m 
--  else let (x, y) = key_to_coord m.target in {m | x <- x, y <- y}

target k m = 
  if key_found k then { m | target <- k } else m

stop_if_target m = 
  if m.target == 32 then {m | vel <- {x = 0, y = 0}}
  else let tpos = key_to_coord m.target in
    if (vabs <| vsub tpos m.pos) < 2.0
    then {m | target <- 32, pos <- tpos}
    else m

target2acc t m = 
  if m.target == 32 then {m | acc <- {x = 0, y = 0}}
  else let tpos = key_to_coord m.target 
           d = vabs (vsub tpos m.pos)
           dir = vmulf (vsub tpos m.pos) (1.0/d)
           new_acc = vmulf dir max_vel
           acc_ratio = (vabs new_acc) / accel
       in { m | acc <- if acc_ratio > 1.0 then vmulf new_acc (1.0/acc_ratio)
                       else new_acc }
       -- in {m | acc <- dir}


acc2vel t m = 
  if m.target == 32 then {m | vel <- {x = 0, y = 0}}
  else let tpos = key_to_coord m.target 
           new_vel = vadd m.vel (vmulf m.acc t)
           vel_ratio = (vabs new_vel) / max_vel
       in { m | vel <- if vel_ratio > 1.0 then vmulf new_vel (1.0/vel_ratio)
                       else new_vel }

vel2pos t m = { m | pos <- vadd m.pos (vmulf m.vel t)}

-- walk {x} m = { m | vx <- 10 * toFloat x}

bounds m = {m | pos <- { x = clamp min_x max_x m.pos.x
                        ,y = clamp min_y max_y m.pos.y } }

step (t, k) m = (bounds . vel2pos t . acc2vel t . target2acc t . stop_if_target . target k) m

------
render (w',h') p1 =
  let (w,h) = (toFloat w', toFloat h')
  in collage w' h' <| concat [
    [toForm (image 1242 400 "https://dl.dropboxusercontent.com/u/18290040/maptt.png") |> move (-50,52)],
    --(map (\{x, y} -> rect (unit*0.8) (unit*0.8) |> outlined (dotted (rgb 150 150 150)) |> move (x, y)) <| concat grid),
    [  
       (if p1.target == 32 
        then rect 0 0 |> filled black 
        else circle 30 |> outlined (dashed (rgb 0 255 88)) |> move (v2p <| key_to_coord p1.target))
      --,(if p1.target == 32 
      --  then rect 0 0 |> filled black 
      --  else traced (solid white) <| segment (v2p p1.pos) (v2p <| key_to_coord p1.target))
      --,rect (unit*0.4) (unit*0.4) |> filled white |> move (v2p p1.pos)
      ,toForm (image 50 50 "https://dl.dropboxusercontent.com/u/18290040/plaball.gif") |> rotate 33 |> move (v2p p1.pos)
      --,traced (solid blue) <| segment (v2p p1.pos) (v2p <| vadd p1.pos <| vmulf p1.vel 100.0)
      ,traced (solid green) <| segment (v2p p1.pos) (v2p <| vsub p1.pos <| vmulf p1.acc 100.0)
      --,toForm (plainText (String.fromList ((Char.fromCode p1.target)::(concat key_rows))))
      --,toForm <| plainText <| String.fromList <| [Char.fromCode c]
    ]
  ]
     

-----
input = let delta = lift (\t -> t/(1000.0/60.0)) (fps 60)
        in sampleOn delta (lift2 (,) delta Keyboard.lastPressed)

----
main  = lift2 render Window.dimensions (foldp step p input)
