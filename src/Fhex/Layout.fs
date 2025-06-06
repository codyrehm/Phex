namespace Fhex

/// <summary>
/// Record <c>Layout</c> represents the layout or structure of a collection of
/// coordinates. It describes things such as their orientation and the size of
/// individual cells, etc.
/// </summary>
[<Struct>]
type Layout = {
    orientation: OrientationMatrix
    size: Point
    origin: Point
}

/// <summary>
/// Module <c>Layout</c> contains functions that operate on <c>Layout</c> types.
/// </summary>
[<RequireQualifiedAccess>]
module Layout =

    /// <summary>
    /// Function <c>create</c> creates a new <c>Layout</c>.
    /// </summary>
    /// <param name="orientation">
    /// Whether the hexes have flat or angled tops
    /// </param>
    /// <param name="size">
    /// The radius in pixels of the hexagon with differnt <c>x</c> and <c>y</c> values indicating
    /// a stretched hexagon
    /// </param>
    /// <param name="origin">
    /// The center of the <c>q</c>, <c>r</c>, and <c>s</c> = 0 hexagon in pixels
    /// </param>
    /// <returns>
    /// The new <c>Layout</c>
    /// </returns>
    let create orientation size origin =
        { orientation = orientation; size = size; origin = origin }

    /// <summary>
    /// Function <c>toPixel</c> converts a <c>Cube</c> to a pixel <c>Point</c> at its origin.
    /// </summary>
    /// <param name="cube">
    /// The <c>Cube</c> coordinate to convert
    /// </param>
    /// <param name="layout">
    /// The layout tha governs this <c>Cube</c>
    /// </param>
    /// <returns>
    /// The origin <c>Point</c> for the <c>Cube</c>
    /// </returns>
    let toPixel (cube: Cube) layout =
        let m = layout.orientation
        let x = (m.f0 * double cube.q + m.f1 * double cube.r) * layout.size.x
        let y = (m.f2 * double cube.q + m.f3 * double cube.r) * layout.size.y
        { x = x + layout.origin.x; y = y + layout.origin.y }

    /// <summary>
    /// Function <c>toCube</c> converts a <c>Point</c> representing a screen pixel
    /// to its corresponding <c>Cube</c> coordinate.
    /// </summary>
    /// <param name="p">
    /// The <c>Point</c> reprensting the pixel whose <c>Cube</c> should be found.
    /// </param>
    /// <param name="layout">
    /// The <c>Layout</c> that governs this <c>Point</c>
    /// </param>
    /// <returns>
    /// The <c>Cube</c> whithin whose bounds the <c>Point</c> falls
    /// </returns>
    let toCube p layout =
        let x = (p.x - layout.origin.x) / layout.size.x
        let y = (p.y - layout.origin.y) / layout.size.y
        let pt = { x = x; y = y }
        let q = layout.orientation.b0 * pt.x + layout.orientation.b1 + pt.y
        let r = layout.orientation.b2 * pt.x + layout.orientation.b3 * pt.y
        Cube.FractionalCube.create q r
        |> Cube.FractionalCube.toCube
