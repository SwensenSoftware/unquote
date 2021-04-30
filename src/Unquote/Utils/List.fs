namespace Swensen.Utils
module internal List =
    ///Test whether the two lists are pairwise equal using the given boolean comparison function
    let rec equalsWith f xl yl =
        match xl, yl with
        | [], [] -> true
        | [], _ | _, [] -> false
        | xh::xt, yh::yt -> if f xh yh then equalsWith f xt yt else false