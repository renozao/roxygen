new_cache <- function() {
  cache <- new.env(parent = emptyenv())
  
  .compute <- function(keys, code) {
    hash <- suppressWarnings(digest(keys))
    if (exists(hash, cache, inherits = FALSE)) {
	  return(cache[[hash]])
    }
    
	if( !missing(code) )
    	(cache[[hash]] <- force(code))
  }
    
  .chain_compute <- function(key, target){
	  
	  	# substitute target by end node and update cache
	  	if( !missing(target) ){			
			target <- .chain_compute(target)
			if( key != target )	.compute(key, target)
			else target
		}else{
			if( is.null(key) ) NULL
			else{
				res <- .compute(key)
				if( is.null(res) ) key
				else{
				#cat("cache_chain: ", key, " - ", res,"\n")	
				.chain_compute(res)
				}
			}
		}
	}
  
  reset <- function() {
    cache <<- new.env(parent = emptyenv())
  }
  
  
  list(compute = .compute, reset = reset, chain_compute = .chain_compute)
}

parse_cache <- new_cache()
rd_proc_cache <- new_cache()
rd_out_cache <- new_cache()
rdname_cache <- new_cache()

#' Clear all roxygen caches.
#'
#' In order to speed up execution time, roxygen caches a number of 
#' interim results. This function empties all caches and guarantees that all
#' results are computed afresh.
#' 
#' @export
clear_caches <- function() {
  parse_cache$reset()
  rd_proc_cache$reset()
  rd_out_cache$reset()
}