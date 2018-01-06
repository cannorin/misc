(*
The X11 License
paket.core.fsx - prepare Paket.Core for import.fsx
Copyright(c) 2018 cannorin
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"

open System
open System.IO
open System.IO.Compression

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
if not (Path.Combine (".paket", "lib", "net45", "Paket.Core.dll") |> File.Exists) then
  let urls = [
    "https://www.nuget.org/api/v2/package/Paket.Core/4.8.8"; 
    "https://www.nuget.org/api/v2/package/Chessie/0.6.0"; 
    "https://www.nuget.org/api/v2/package/Mono.Cecil/0.9.6.4"; 
    "https://www.nuget.org/api/v2/package/Newtonsoft.Json/10.0.3"
    ] 
  in
  let prepare (url: string) =
    use wc = new Net.WebClient() in
    let tmp = Path.GetTempFileName() in
    wc.DownloadFile(url, tmp);
    use nupkg = ZipFile.OpenRead tmp in
    for entry in nupkg.Entries do
      let path = Path.Combine(".paket", entry.FullName) in
      Path.GetDirectoryName path |> Directory.CreateDirectory |> ignore;
      entry.ExtractToFile(path, true)
    done
  in
  urls |> List.iter prepare;
  Path.Combine(".paket", "load", "net45") |> Directory.CreateDirectory |> ignore
;;

