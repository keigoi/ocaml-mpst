
module Local : S.LOCAL
module MPST : S.GLOBAL with type 'a sess = 'a Local.sess
