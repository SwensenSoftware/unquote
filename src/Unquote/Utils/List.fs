(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
namespace Swensen.Utils
module internal List =
    ///Test whether the two lists are pairwise equal using the given boolean comparison function
    let rec equalsWith f xl yl =
        match xl, yl with
        | [], [] -> true
        | [], _ | _, [] -> false
        | xh::xt, yh::yt -> if f xh yh then equalsWith f xt yt else false