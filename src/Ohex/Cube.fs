namespace Ohex

open Fhex

/// <summary>
/// Class <c>Cube</c> is a hexagon represented as a cube in a three
/// dimensional space with three coordinates to locat it:
/// Q (column)
/// R (row)
/// S (z-coordinate)
/// </summary>
type Cube (q: int, r: int) =

    let _q = q
    let _r = r
    let _s = -q - r

    /// <summary>
    /// Constructor that creates a new <c>Cube</c> object
    /// from the corresponding <c>Cube</c> record type.
    /// </summary>
    new (other: Fhex.Cube) =
        Cube(other.q, other.r)

    /// <summary>
    /// Property <c>Q</c> is the q or column coordinate for this cube.
    /// </summary>
    member this.Q = _q

    /// <summary>
    /// Property <c>R<c> is the r or row coordinate for this cube.
    /// </summary>
    member this.R = _r

    /// <summary>
    /// Property <c>S</c> is the z coordinate for this cube.
    /// </summary>
    member this.S = _s

    /// <summary>
    /// Method <c>Add</c> adds one <c>Cube</c> to another.
    /// </summary>
    /// <param name="other">
    /// The <c>Cube</c> to add to this one.
    /// </param>
    /// <returns>
    /// A new <c>Cube</c> that is the result of the addition.
    /// </returns>
    member this.Add (other: Cube): Cube =
        let a = Cube.create this.Q this.R
        let b = Cube.create other.Q other.R
        Cube(Cube.add a b)

    /// <summary>
    /// Method <c>Subtract</c> subtracts one <c>Cube</c> from another.
    /// </summary>
    /// <param name="other">
    /// The <c>Cube</c> to subtract.
    /// </param>
    /// <returns>
    /// A new <c>Cube</c> that is the result of the subtraction.
    /// </returns>
    member this.Subtract (other: Cube): Cube =
        let a = Cube.create this.Q this.R
        let b = Cube.create other.Q other.R
        Cube(Cube.sub a b)

    /// <summary>
    /// Method <c>Multiply</c> multiplies this <c>Cube</c> coordinate by a factor.
    /// </summary>
    /// <param name="factor">
    /// The factor by which to scale this <c>Cube</c>.
    /// </param>
    /// <returns>
    /// The scaled <c>Cube</c>.
    /// </returns>
    member this.Multiply (factor: int): Cube =
        let a = Cube.create this.Q this.R
        Cube(Cube.multiply a factor)

    member this.Distance (other: Cube): int =
        let a = Cube.create this.Q this.R
        let b = Cube.create other.Q other.R
        Cube.dist a b
