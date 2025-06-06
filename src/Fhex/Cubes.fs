namespace Fhex

/// <summary>
/// Module <c>Cubes</c> contains functions that operate on collections
/// of <c>Cube</c> coordinates.
/// </summary>
module Cubes =

    /// <summary>
    /// Function <c>createRectangle</c> creates a rectangular shaped
    /// map of <c>Cubes</c>.
    /// </summary>
    /// <param name="orientation">
    /// The orientation of the <c>Cubes</c>. Either a flat or angled top.
    /// </param>
    /// <param name="top">
    /// The coordinate of the top-most row of <c>Cubes</c>.
    /// </param>
    /// <param name="bottom">
    /// The coordinate of the bottom-most row of <c>Cubes</c>.
    /// </param>
    /// <param name="left">
    /// The coordinate of the left-most column of <c>Cubes</c>.
    /// </param>
    /// <param name="right">
    /// The coordinate of the right-most column of <c>Cubes</c>.
    /// </param>
    /// <returns>
    /// A sequence of <c>Cubes</c> comprising a rectangular map of the
    /// given dimensions.
    /// </returns>
    let createRectangle orientation top bottom left right =
        let offset x =
            int (floor (float x / 2.0))

        let angled =
            let row r = seq { for q in left - offset r..right - offset r do yield Cube.create q r }
            seq { for r in top..bottom do yield row r } |> Seq.collect id

        let flat =
            let column q = seq { for r in top - offset q..bottom - offset q do yield Cube.create q r }
            seq { for q in left..right do yield column q } |> Seq.collect id

        match orientation with
        | Orientation.Angled -> angled
        | Orientation.Flat -> flat

    /// <summary>
    /// Function <c>neighbor</c> gives the cube that is adjacent
    /// to the indicated cube in the direction specified.
    /// </summary>
    /// <param name="cube">
    /// The origin cube whose neighbor is sought.
    /// </param>
    /// <param name="dir">
    /// The direction of the adjacent neighbor to find.
    /// Increasing clockwise.
    /// </param>
    /// <param name="cubes">
    /// The list of cubes within which to search.
    /// </param>
    /// <returns>
    /// An optional cube if one is found.
    /// </return>
    let neighbor (cube: Cube) (dir: int) (cubes: Cube seq): Cube option =
        let targetCube = cube |> Cube.neighbor dir
        let hasCube = cubes |> Seq.contains cube
        let hasTarget = cubes |> Seq.contains targetCube

        match hasCube, hasTarget with
        | true, true -> Some targetCube
        | _ -> None
