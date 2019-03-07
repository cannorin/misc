module SpanUtils

open System
open System.Collections.Generic
open FSharp.NativeInterop

#nowarn "9"
type span<'a> = Span<'a>
type readonlyspan<'a> = ReadOnlySpan<'a>
type stringspan = readonlyspan<char>

module SafeLowlevelOperators =
  open System.Runtime.InteropServices
  let inline malloc<'a> length =
    let xs = Array.zeroCreate length
    span<'a>(xs)
  let inline stackalloc<'a when 'a: unmanaged> length =
    let mem = NativePtr.stackalloc<'a> length
    span<'a>(mem |> NativePtr.toVoidPtr, length)
  let inline nativealloc<'a when 'a: unmanaged> length =
    let mem = Marshal.AllocCoTaskMem(Marshal.SizeOf<'a>() * length)
    span<'a>(mem.ToPointer(), length), fun () -> Marshal.FreeCoTaskMem(mem)

module Span =
  let inline ofArray (xs: _[]) = span(xs)
  let inline ofPtr (p: nativeptr<'a>, size) =
    span<'a>(p |> NativePtr.toVoidPtr, size)
  let inline ofMemory (mem: Memory<'a>) = mem.Span
  let inline toArray (s: span<_>) = s.ToArray()

  let inline isEmpty (s: span<_>) = s.IsEmpty
  let inline length (s: span<_>) = s.Length
  let inline item i (s: span<_>) = s.[i]
  let inline rev (s: span<_>) = s.Reverse()
  let inline take (i: int) (s: span<_>) = s.Slice(0, i-1)
  let inline skip (i: int) (s: span<_>) = s.Slice(i)
  let inline findIndex (x: 'a) (s: span<'a>) = MemoryExtensions.IndexOf(s, x)
  let inline findIndexOfSpan (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.IndexOf(s, xs)
  let inline findIndexOfAnyOf2 (x1: 'a) (x2: 'a) (s: span<'a>) = MemoryExtensions.IndexOfAny(s, x1, x2)
  let inline findIndexOfAnyOf3 (x1: 'a) (x2: 'a) (x3: 'a) (s: span<'a>) = MemoryExtensions.IndexOfAny(s, x1, x2, x3)
  let inline findIndexOfAnyOf (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.IndexOfAny(s, xs)
  let inline findLastIndex (x: 'a) (s: span<'a>) = MemoryExtensions.LastIndexOf(s, x)
  let inline findLastIndexOfSpan (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.LastIndexOf(s, xs)
  let inline findLastIndexOfAnyOf2 (x1: 'a) (x2: 'a) (s: span<'a>) = MemoryExtensions.LastIndexOfAny(s, x1, x2)
  let inline findLastIndexOfAnyOf3 (x1: 'a) (x2: 'a) (x3: 'a) (s: span<'a>) = MemoryExtensions.LastIndexOfAny(s, x1, x2, x3)
  let inline findLastIndexOfAnyOf (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.LastIndexOfAny(s, xs)
  let inline overlaps (other: readonlyspan<_>) (s: span<_>) = s.Overlaps(other)
  let inline sequenceCompareTo (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.SequenceCompareTo(s, xs)
  let inline sequenceEqual (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.SequenceEqual(s, xs)
  let inline startsWith (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.StartsWith(s, xs)
  let inline endsWith (xs: readonlyspan<'a>) (s: span<'a>) = MemoryExtensions.EndsWith(s, xs)

  let inline fillWith x (s: span<_>) = s.Fill(x)
  let inline binarySearch<'a when 'a :> IComparable<'a>> (x: 'a) (s: span<'a>) =
    s.BinarySearch(x :> IComparable<_>)
  let inline binarySearchBy (comparer: 'a -> 'a -> int) (x: 'a) (s: span<'a>) =
    s.BinarySearch(x, Comparer.Create(Comparison(comparer)))

module ReadOnlySpan =
  let inline ofArray (xs: _[]) = readonlyspan(xs)
  let inline ofPtr (p: nativeptr<'a>, size) =
    readonlyspan<'a>(p |> NativePtr.toVoidPtr, size)
  let inline ofMemory (mem: ReadOnlyMemory<'a>) = mem.Span
  let inline ofString (s: string) = s.AsSpan()

  let inline toArray (s: readonlyspan<_>) = s.ToArray()
  
  let inline isEmpty (s: readonlyspan<_>) = s.IsEmpty
  let inline length (s: readonlyspan<_>) = s.Length
  let inline item i (s: readonlyspan<_>) = s.[i]
  let inline take (i: int) (s: readonlyspan<_>) = s.Slice(0, i-1)
  let inline skip (i: int) (s: readonlyspan<_>) = s.Slice(i)
  let inline findIndex (x: 'a) (s: readonlyspan<'a>) = MemoryExtensions.IndexOf(s, x)
  let inline findIndexOfSpan (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.IndexOf(s, xs)
  let inline findIndexOfAnyOf2 (x1: 'a) (x2: 'a) (s: readonlyspan<'a>) = MemoryExtensions.IndexOfAny(s, x1, x2)
  let inline findIndexOfAnyOf3 (x1: 'a) (x2: 'a) (x3: 'a) (s: readonlyspan<'a>) = MemoryExtensions.IndexOfAny(s, x1, x2, x3)
  let inline findIndexOfAnyOf (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.IndexOfAny(s, xs)
  let inline findLastIndex (x: 'a) (s: readonlyspan<'a>) = MemoryExtensions.LastIndexOf(s, x)
  let inline findLastIndexOfSpan (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.LastIndexOf(s, xs)
  let inline findLastIndexOfAnyOf2 (x1: 'a) (x2: 'a) (s: readonlyspan<'a>) = MemoryExtensions.LastIndexOfAny(s, x1, x2)
  let inline findLastIndexOfAnyOf3 (x1: 'a) (x2: 'a) (x3: 'a) (s: readonlyspan<'a>) = MemoryExtensions.LastIndexOfAny(s, x1, x2, x3)
  let inline findLastIndexOfAnyOf (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.LastIndexOfAny(s, xs)
  let inline overlaps (other: readonlyspan<_>) (s: readonlyspan<_>) = s.Overlaps(other)
  let inline sequenceCompareTo (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.SequenceCompareTo(s, xs)
  let inline sequenceEqual (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.SequenceEqual(s, xs)
  let inline startsWith (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.StartsWith(s, xs)
  let inline endsWith (xs: readonlyspan<'a>) (s: readonlyspan<'a>) = MemoryExtensions.EndsWith(s, xs)

  let inline binarySearch<'a when 'a :> IComparable<'a>> (x: 'a) (s: readonlyspan<'a>) =
    s.BinarySearch(x :> IComparable<_>)
  let inline binarySearchBy (comparer: 'a -> 'a -> int) (x: 'a) (s: readonlyspan<'a>) =
    s.BinarySearch(x, Comparer.Create(Comparison(comparer)))

type Span<'a> with
  member inline this.GetSlice(startIndex: int option, endIndex: int option) =
    let s = defaultArg startIndex 0
    if endIndex.IsSome then
      this.Slice(s, endIndex.Value - s)
    else
      this.Slice(s)
  member inline this.Item
    with get (i: int) = Span.item i this
    and set (i: int) (v: _) =
      let r : byref<_> = &this.[i]
      r <- v
  member inline this.get i = this.[i]
  member inline this.set i x = this.[i] <- x
    
type ReadOnlySpan<'a> with
  member inline this.GetSlice(startIndex: int option, endIndex: int option) =
    let s = defaultArg startIndex 0
    if endIndex.IsSome then
      this.Slice(s, endIndex.Value - s)
    else
      this.Slice(s)
  member inline this.get i = this.[i]

module StringSpan =
  let inline ofString (s: string) : stringspan = s.AsSpan()
  let inline toString (s: stringspan) = s.ToString()

  let inline isEmpty (s: stringspan) = s.IsEmpty
  let inline length (s: stringspan) = s.Length
  let inline item i (s: stringspan) = s.[i]
  let inline take (i: int) (s: stringspan) : stringspan = s.Slice(0, i-1)
  let inline skip (i: int) (s: stringspan) : stringspan = s.Slice(i)
  let inline findIndex (x: char) (s: stringspan) = MemoryExtensions.IndexOf(s, x)
  let inline findIndexOfSpan (xs: stringspan) (s: stringspan) = MemoryExtensions.IndexOf(s, xs)
  let inline findIndexOfAnyOf2 (x1: char) (x2: char) (s: stringspan) = MemoryExtensions.IndexOfAny(s, x1, x2)
  let inline findIndexOfAnyOf3 (x1: char) (x2: char) (x3: char) (s: stringspan) = MemoryExtensions.IndexOfAny(s, x1, x2, x3)
  let inline findIndexOfAnyOf (xs: stringspan) (s: stringspan) = MemoryExtensions.IndexOfAny(s, xs)
  let inline findLastIndex (x: char) (s: stringspan) = MemoryExtensions.LastIndexOf(s, x)
  let inline findLastIndexOfSpan (xs: stringspan) (s: stringspan) = MemoryExtensions.LastIndexOf(s, xs)
  let inline findLastIndexOfAnyOf2 (x1: char) (x2: char) (s: stringspan) = MemoryExtensions.LastIndexOfAny(s, x1, x2)
  let inline findLastIndexOfAnyOf3 (x1: char) (x2: char) (x3: char) (s: stringspan) = MemoryExtensions.LastIndexOfAny(s, x1, x2, x3)
  let inline findLastIndexOfAnyOf (xs: stringspan) (s: stringspan) = MemoryExtensions.LastIndexOfAny(s, xs)
  let inline overlaps (other: stringspan) (s: stringspan) = s.Overlaps(other)
  let inline sequenceCompareTo (other: stringspan) (s: stringspan) = MemoryExtensions.SequenceCompareTo(s, other)
  let inline sequenceEqual (other: stringspan) (s: stringspan) = MemoryExtensions.SequenceEqual(s, other)

  let inline substring startIndex endIndex (s: stringspan) = s.Slice(startIndex, endIndex - startIndex)

  [<Literal>]
  let DefaultComparison =
    #if !NETSTANDARD1_6
    StringComparison.InvariantCulture
    #else
    StringComparison.CurrentCulture
    #endif

  let inline compare (other: stringspan) (s: stringspan) =
    MemoryExtensions.CompareTo(s, other, DefaultComparison)
  let inline compareOrdinal (other: stringspan) (s: stringspan) =
    MemoryExtensions.CompareTo(s, other, StringComparison.Ordinal)
  let inline compareWithComparison (cmp: StringComparison) (other: stringspan) (s: stringspan) =
    MemoryExtensions.CompareTo(s, other, cmp)
  let inline contains (other: stringspan) (s: stringspan) =
    MemoryExtensions.Contains(s, other, DefaultComparison)
  let inline containsOrdinal (other: stringspan) (s: stringspan) =
    MemoryExtensions.Contains(s, other, StringComparison.Ordinal)
  let inline containsWithComparison (cmp: StringComparison) (other: stringspan) (s: stringspan) =
    MemoryExtensions.Contains(s, other, cmp)
  let inline equals (other: stringspan) (s: stringspan) =
    MemoryExtensions.Equals(s, other, DefaultComparison)
  let inline equalsOrdinal (other: stringspan) (s: stringspan) =
    MemoryExtensions.Equals(s, other, StringComparison.Ordinal)
  let inline equalsWithComparison (cmp: StringComparison) (other: stringspan) (s: stringspan) =
    MemoryExtensions.Equals(s, other, cmp)
  let inline startsWith (other: stringspan) (s: stringspan) =
    MemoryExtensions.StartsWith(s, other, DefaultComparison)
  let inline startsWithOrdinal (other: stringspan) (s: stringspan) =
    MemoryExtensions.StartsWith(s, other, StringComparison.Ordinal)
  let inline startsWithComparison (cmp: StringComparison) (other: stringspan) (s: stringspan) =
    MemoryExtensions.StartsWith(s, other, cmp)
  let inline endsWith (other: stringspan) (s: stringspan) =
    MemoryExtensions.EndsWith(s, other, DefaultComparison)
  let inline endsWithOrdinal (other: stringspan) (s: stringspan) =
    MemoryExtensions.EndsWith(s, other, StringComparison.Ordinal)
  let inline endsWithComparison (cmp: StringComparison) (other: stringspan) (s: stringspan) =
    MemoryExtensions.EndsWith(s, other, cmp)

  let inline binarySearch (x: char) (s: stringspan) =
    s.BinarySearch(x :> IComparable<_>)
  let inline binarySearchBy (comparer: char -> char -> int) (x: char) (s: stringspan) =
    s.BinarySearch(x, Comparer.Create(Comparison(comparer)))

module String =
  let inline toSpan (s: string) : stringspan = s.AsSpan()
