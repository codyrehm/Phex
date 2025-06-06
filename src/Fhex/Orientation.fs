namespace Fhex

/// <summary>
/// Record <c>OrientationMatrix</c> contains the values necessary to perform
/// the different mathmatical matrix operations by a <c>Layout</c> when doing things
/// like translating pixel coordinates on a screen to an actual hex coordinate.
/// </summary>
[<Struct>]
type OrientationMatrix = {
    angle: double
    f0: double
    f1: double
    f2: double
    f3: double
    b0: double
    b1: double
    b2: double
    b3: double
}

/// <summary>
/// Union <c>Orientation</c> enumerates the two possible positionings that a
/// hexagon may have. Either an angled, "pointy" top or a flat one.
/// </summary>
[<RequireQualifiedAccess>]
type Orientation =
    | Angled
    | Flat

/// <summary>
/// Module <c>Orientation</c> contains functions that handle operations on the
/// <c>OrientationMatrix</c> type.
/// </summary>
[<RequireQualifiedAccess>]
module Orientation =

    /// <summary>
    /// Value <c>angled</c> is the <c>OrientationMatrix</c> that describes hexagons
    /// whose tops are angled or "pointy".
    /// </summary>
    let angled = {
        angle = 0.5
        f0 = sqrt 3.0
        f1 = sqrt 3.0 / 2.0
        f2 = 0.0
        f3 = 3.0 / 2.0
        b0 = sqrt 3.0 / 3.0
        b1 = -1.0 / 3.0
        b2 = 0.0
        b3 = 2.0 / 3.0
    }

    /// <summary>
    /// Value <c>flat</c> is the <c>OrientationMatrix</c> that describes hexagons
    /// whose tops are flat as opposed to angled.
    /// </summary>
    let flat = {
        angle = 0.0
        f0 = 3.0 / 2.0
        f1 = 0.0
        f2 = sqrt 3.0 / 2.0
        f3 = sqrt 3.0
        b0 = 2.0 / 3.0
        b1 = 0.0
        b2 = -1.0 / 3.0
        b3 = sqrt 3.0 / 3.0
    }

    /// <summary>
    /// Function <c>parse</c> uses the provided string to create either an angled or
    /// flat top orientation.
    /// </summary>
    /// <param name="str">
    /// The string that should be parsed must be one of <c>angled</c> or <c>flat</c>
    /// </param>
    /// <returns>
    /// An <c>Orientation</c>
    /// </returns>
    let parse (str: string) =
        match str.ToLower() with
        | "flat" -> flat
        | "angled" -> angled
        | _ -> invalidArg "str" "Could not parse orientation; valid values are flat or angled"
