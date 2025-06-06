namespace Fhex

/// <summary>
/// Record <c>Point</c> is a standard two-dimensional
/// coordinate.
/// </summary>
[<Struct>]
type Point = {
    x: double
    y: double
}

/// <summary>
/// Module <c>Point</c> contains functions that operate
/// on the <c>Point</c> type.
/// </summary>
[<RequireQualifiedAccess>]
module Point =

    /// <summary>
    /// Function <c>create</c> creates a new <c>Point</c>.
    /// <summary>
    /// <param name="x">
    /// The x coordinate.
    /// </param>
    /// <param name="y">
    /// The y coordinate.
    /// </param>
    /// <returns>
    /// A new <c>Point</c>.
    /// </returns>
    let create x y =
        { x = x; y = y }

    /// <summary>
    /// Function <c>zero</c> give the zero-value <c>Point</c> (origin).
    /// </summary>
    /// <returns>
    /// A point at 0, 0, the origin.
    /// </returns>
    let zero =
        { x = 0.0; y = 0.0 }
