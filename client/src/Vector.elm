module Vector exposing (Vector, add, contains, mul, sub)


type alias Vector =
    { x : Float, y : Float }


add : Vector -> Vector -> Vector
add p0 p1 =
    { x = p0.x + p1.x, y = p0.y + p1.y }


sub : Vector -> Vector -> Vector
sub p0 p1 =
    { x = p0.x - p1.x, y = p0.y - p1.y }


mul : Float -> Vector -> Vector
mul k p =
    { x = k * p.x, y = k * p.y }


contains : Vector -> Vector -> Vector -> Bool
contains { x, y } p0 p1 =
    p0.x <= x && x < p1.x && p0.y <= y && y < p1.y
