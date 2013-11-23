
import NeatInterpolation.Prelude
import NeatInterpolation

main = do
  let a' = [string| 
    function startsWith( start, string ){
      return string.lastIndexOf( start ) == 0
    }
  |]
  let pattern = [string| {
      

      $a'

      single inlining: ( $a' )


      {
        multiple inlining: ( $a' ) ( $a' )
      }


    } 
    |]
  putStrLn pattern
