colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
     = colors[iris$Species], asp = 1)
xl <- iris[, 3:5] ## выборка
eD <- function(u, v)  ##функция расстояний, эвклидово расстояние между функциями
        sqrt(sum((u - v)^2))
}
sort <- function(xl, point)  ## сортировка объектов по возрастанию согласно растояния до объекта point
{
{
        l <- dim(xl)[1]
        n <- dim(xl)[2] - 1
        distances <- matrix(NA, l, 2)
        for (i in 1:l)
        {
                distances[i, ] <- c(i,eD(xl[i, 1:n], point)) ## расстояние от каждой точки до точки point, классифицируемой
        }
        newXl <- xl[order(distances[, 2]), ] ## сортировка выборки
}
NN1 <- function(xl, point)
{
        newXl <- sort(xl, point)
        n <- dim(newXl)[2] - 1 
        class <- newXl[1, n + 1] ## получение класса соседа
        return (class) ## возвращаем класс
}

point <- c(4.3, 2.0)
class <- NN1(xl, point)
points(point[1], point[2], pch = 21, bg = colors[class], asp = 1)

