open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

open LibGit2Sharp

[<EntryPoint>]
let main args =
//  Environment.CurrentDirectory <- @"C:\code\github\kodelife\tools\FsPrependGitFiles\FsPrependGitFiles"
  let repoPath = Path.GetFullPath @"../../../"

  printfn "Opening repo: %s" repoPath
  use repo = new Repository (repoPath)

  let firstSeen = Dictionary<string, string> ()

  let cleanPath (p : string) =
    let p = p.ToLowerInvariant()
    let p = p.Replace ('\\', '/')
    p

  let rec traverse dt (te : TreeEntry) =
    match te.TargetType with
    | TreeEntryTargetType.Tree ->
      let tree : Tree = te.Target :?> Tree
      for tte in tree do
        traverse dt tte
      ()
    | TreeEntryTargetType.Blob  ->
      firstSeen.[cleanPath te.Path] <- dt
    | _                         ->
      failwithf "Unsupported target type: %A" te.TargetType

  printfn "Traversing all commits"
  for commit in repo.Head.Commits do
    let committed = commit.Committer.When
    let dt = sprintf "%04d%02d%02d" committed.Year committed.Month committed.Day
    for te in commit.Tree do
      traverse dt te

  printfn "Finding *.klproj files in src"
  let srcPath = Path.GetFullPath @"../../../src"
  let klprojFiles = Directory.GetFiles (srcPath, "*.klproj", SearchOption.TopDirectoryOnly)

  let rDate = Regex "^\d{8}-"

  printfn "Prepending found *.klproj file with creation date"
  for klprojFile in klprojFiles do
    let fn  = Path.GetFileName klprojFile
    let fp  = klprojFile.Substring repoPath.Length
    let cfp = cleanPath fp
    let mDate = rDate.Match fn
    if not mDate.Success then
      match firstSeen.TryGetValue cfp with
      | true  , dt  -> 
        let dn = Path.GetDirectoryName klprojFile
        let np = Path.Combine (dn, sprintf "%s-%s" dt fn)
        File.Move (klprojFile, np)
      | false , _   -> printfn "WARN: Not found in git history: %s" fp
  0