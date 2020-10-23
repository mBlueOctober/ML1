colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
     = colors[iris$Species], asp = 1)
xl <- iris[, 3:5] ## �������
eD <- function(u, v)  ##������� ����������, ��������� ���������� ����� ���������
{
  sqrt(sum((u - v)^2))
}
sort <- function(xl, point)  ## ���������� �������� �� ����������� �������� ��������� �� ������� point
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i,eD(xl[i, 1:n], point)) ## ���������� �� ������ ����� �� ����� point, ����������������
  }
  newXl <- xl[order(distances[, 2]), ] ## ���������� �������
}
kNN <- function(xl, point, k)
{
  newXl <- sort(xl, point)
  n <- dim(newXl)[2] - 1 
  class <- newXl[1:k, n + 1] ## ��������� ������ ������
  return (class)## ���������� �����
}

point <- c(4.3, 2.0)
class <- kNN(xl, point, k=6)
points(point[1], point[2], pch = 21, bg = colors[class], asp = 1)
