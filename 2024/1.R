input <- read.delim("2024/input1.txt", sep = "", header = FALSE)

# P1
sum(abs(sort(input$V1)-sort(input$V2)))

# P2

sum(
  sapply(
    input$V1,
    function(x){
      x * sum(input$V2 == x)
    }
  )
)
