namespace Fhex

/// <summary>
/// Record <c>FractionalCube</c> is cube coordinate that uses
/// double for the coordinates rather than integers.
/// </summary>
type FractionalCube = {
    q: double
    r: double
    s: double
}

/// <summary>
/// Record <c>Cube</c> represents a hexagon using a three
/// point coordinate system with q (column) r (row) and
/// s (z-coordinate).
/// </summary>
type Cube = {
    q: int
    r: int
    s: int
}

/// <summary>
/// Module <c>Cube</c> contains functions that operate on
/// the <c>Cube</c> coordidnate type.
/// </summary>
[<RequireQualifiedAccess>]
module rec Cube =

    /// <summary>
    /// Module <c>FractionalCube</c> contains functions that operate on
    /// <c>FractionalCubes</c>.
    /// </summary>
    [<RequireQualifiedAccess>]
    module FractionalCube =

        /// <summary>
        /// Function <c>create</c> creates a new <c>FractionalCube</c>.
        /// The value of the s (z) coordinate will be set to -q-r.
        /// </summary>
        /// <param name="q">
        /// The q (or column coordinate).
        /// </param>
        /// <param name="r">
        /// The r (or row coordinate).
        /// </param>
        /// <returns>
        /// A new <c>FractionalCube</c>.
        /// </returns>
        let create (q: double) (r: double): FractionalCube =
            { q = q; r = r; s = -q-r }

        /// <summary>
        /// Function <c>toCube</c> converts a <c>FractionalCube</c>
        /// to a <c>Cube</c>.
        /// </summary>
        /// <param name="h">
        /// The <c>FractionalCube</c> to convert.
        /// </param>
        /// <returns>
        /// A <c>Cube</c>.
        /// </returns>
        let toCube (h: FractionalCube): Cube =
            let q = round h.q
            let r = round h.r
            let s = round h.s

            let qDiff = abs (q - h.q)
            let rDiff = abs (r - h.r)
            let sDiff = abs (s - h.s)

            if qDiff > rDiff && qDiff > sDiff then
                { q = int (-r - s); r = int r; s = int s }
            else if rDiff > sDiff then
                { q = int q; r = int (-q - s); s = int s }
            else
                { q = int q; r = int r; s = int (-q - r) }

        /// <summary>
        /// Function <c>lerp</c> is the linear inerpolation between
        /// two endpoint <c>FractionalCubes</c>.
        /// </summary>
        /// <param name="a">
        /// One of the endpoint <c>FractionalCubes</c>.
        /// </param>
        /// <param name="b">
        /// One of the endpoint <c>FractionalCubes</c>.
        /// </param>
        /// <returns>
        /// The lerp <c>FractionalCube</c>.
        /// </returns>
        let lerp (a: FractionalCube) (b: FractionalCube) (t: double): FractionalCube =
            let q = a.q * (1.0 - t) + b.q * t
            let r = a.r * (1.0 - t) + b.r * t
            let s = a.s * (1.0 - t) + b.s * t
            { q = q; r = r; s = s }

    /// <summary>
    /// Function <c>create</c> creates a new <c>Coord</c> type.
    /// </summary>
    /// <param name="q">
    /// The q or column coordinate.
    /// </param>
    /// <param name="r">
    /// The r or row coordinate.
    /// </param>
    /// <returns>
    /// A new <c>Coord</c> with the s (z) coordinate set to
    /// s = -q-r.
    /// </returns>
    let create q r =
        { q = q; r = r; s = -q-r }

    /// <summary>
    /// Function <c>zero</c> creates a new <c>Coord</c> with
    /// all values initilized to zero.
    /// </summary>
    let zero =
        create 0 0

    /// <summary>
    /// Function <c>add</c> adds two <c>Coords</c> together and
    /// returns a new result <c>Coord</c>.
    /// </summary>
    /// <param name="a">
    /// The first addend <c>Coord</c>.
    /// </param>
    /// <param name="b">
    /// The second addend <c>Coord</c>.
    /// </param>
    /// <returns>
    /// The sum <c>Coord</c>.
    /// </returns>
    let add a b =
        create (a.q + b.q) (a.r + b.r)

    /// <summary>
    /// Function <c>len</c> calculates the length of a <c>Coord</c>.
    /// </summary>
    /// <param name="a">
    /// The <c>Coord</c> whose length should be calculated.
    /// </param>
    /// <returns>
    /// The length of the <c>Coord</c>.
    /// </returns>
    let len a =
        (abs a.q + abs a.r + abs a.s) / 2

    /// <summary>
    /// Function <c>sub</c> subtracts one <c>Cube</c> from another.
    /// </summary>
    /// <param name="a">
    /// The minuend <c>Cube</c>.
    /// </param>
    /// <param name="b">
    /// The subtrahend <c>Cube</c>.
    /// </param>
    /// <returns>
    /// The difference <c>Cube</c>.
    /// </returns>
    let sub a b =
        create (a.q - b.q) (a.r - b.r)

    /// <summary>
    /// Function <c>multiply</c> multiplies one <c>Cube</c> by another.
    /// </summary>
    /// <param name="a">
    /// The multiplicand <c>Cube</c>.
    /// </param>
    /// <param name="b">
    /// The multiplier <c>Cube</c>.
    /// </param>
    /// <returns>
    /// The product <c>Cube</c>.
    /// </returns>
    let multiply a b =
        create (a.q * b) (a.r * b)

    /// <summary>
    /// Function <c>dist</c> calculates the distance between two
    /// <c>Cubes</c>.
    /// </summary>
    /// <param name="a">
    /// The first <c>Cube</c>.
    /// </param>
    /// <param name="b">
    /// The second <c>Cube</c>.
    /// </param>
    /// <returns>
    /// The distance between the two <c>Cubes</c>.
    /// </returns>
    let dist a b =
        sub a b |> len

    let rec private direction = function
        | 0 -> create 0 1
        | 1 -> create 1 -1
        | 2 -> create 1 0
        | 3 -> create 0 1
        | 4 -> create -1 1
        | 5 -> create -1 0
        | _ as x -> direction (x % 6)

    /// <summary>
    /// Function <c>neighbor</c> gives the <c>Cube</c> that is
    /// adjacent to this <c>Cube</c>.
    /// </summary>
    /// <param name="cube">
    /// The <c>Cube</c> whoese neighbor should be found.
    /// </param>
    /// <param name="dir">
    /// A number representing the number of clockwise hex-faces
    /// to traverse in order to find the neighbor. Start at 0.
    /// </param>
    /// <returns>
    /// The <c>Cube</c> adjecent in the direction specified.
    /// </returns>
    let neighbor dir cube =
        direction dir
        |> add cube

    /// <summary>
    /// Function <c>line</c> is the list of cubes that lie between
    /// two endpoint <c>Cubes</c>.
    /// </summary>
    /// <param name="a">
    /// The starting point <c>Cube</c>.
    /// </param>
    /// <param name="b">
    /// The ending point <c>Cube</c>.
    /// </param>
    /// <returns>
    /// A list of <c>Cubes</c> the line between the two points passes
    /// through.
    /// </returns>
    let line a b =
        let n = dist a b
        let aNudge = FractionalCube.create (float a.q + 1e-06) (float a.r + 1e-06)
        let bNudge = FractionalCube.create (float b.q + 1e-06) (float b.r + 1e-06)
        let step = 1.0 / float (max n 1)
        let calculateCube i =
            FractionalCube.lerp aNudge bNudge (step * i)
            |> FractionalCube.toCube
        [ for i in 0..n do yield calculateCube (float i) ]

    /// <summary>
    /// Function <c>str</c> provides a <c>Cube</c> in string format for
    /// display to a terminal for exmaple.
    /// </summary>
    /// <param name="cube">
    /// The <c>Cube</c> that should be converted to a string representation.
    /// </param>
    /// <returns>
    /// A string representation of the <c>Cube</c>.
    /// </returns>
    let str cube =
        sprintf "q: %d r: %d s: %d" cube.q cube.r cube.s
