namespace Ploeh.Samples

open System
open System.IO

type PhotoFile = { File : FileInfo; TakenOn : DateTime }

type Move = { Source : FileInfo; Destination : FileInfo }

module Archive =
    let moveTo destination t =
        let dirNameOf (dt : DateTime) = sprintf "%d-%02d" dt.Year dt.Month
        let groupByDir pf m =
            let key = dirNameOf pf.TakenOn
            let dir = Map.tryFind key m |> Option.defaultValue []
            Map.add key (pf.File :: dir) m
        let addDir name files dirs =
            Tree.node name (List.map Leaf files) :: dirs

        let m = Tree.foldBack groupByDir t Map.empty
        Map.foldBack addDir m [] |> Tree.node destination

    let calculateMoves =
        let replaceDirectory (fi : FileInfo) d =
            FileInfo (Path.Combine (d, fi.Name))
        let rec imp pathToParent = function
            | Leaf fi ->
                Leaf { Source = fi; Destination = replaceDirectory fi pathToParent }
            | Node (directoryName, trees) ->
                let pathToSelf = Path.Combine (pathToParent, directoryName)
                Tree.node pathToSelf (List.map (imp pathToSelf) trees)
        imp ""