# Exercise 1
factorial <- function(n){
  
  if (n == 0){
    return (1)
  }
  
  result = 1
  
  for (i in 1:n){
    result = result*i
  }
  
  return (result)
}

user_factorial <- function(){
  n = as.integer(readline('Please enter a positive number: '))

  while (n < 0){
    print("The number should be positive integer.")
    n = as.integer(readline('Please enter a positive number: '))
  }

  factorial(n)
}

user_factorial()


# Exercise 2
star_print <- function(n1){
  
  for (i in 1:n1){
    print(strrep('*', i), quote = FALSE)
  }
  
}
star_print(5)