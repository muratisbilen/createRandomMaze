checkSurrounding = function(x,y,mat){
	cleft = (2*(x-1) > 0) && (mat[2*(x-1),2*y] == 0)
	cright = (2*(x+1) < ncol(mat)) && (mat[2*(x+1),2*y] == 0)
	cup = (2*(y-1) > 0) && (mat[2*x,2*(y-1)] == 0)
	cdown = (2*(y+1) < nrow(mat)) && (mat[2*x,2*(y+1)] == 0)
	
	dirs = c("l","r","u","d")
	chs = c(cleft,cright,cup,cdown)
	
	return(dirs[which(chs)])
}

coln = 20
rown = 20

a = matrix(1,ncol=2*coln+1,nrow=2*rown+1)

for(i in seq(2,nrow(a),by=2)){
	for(j in seq(2,ncol(a),by=2)){
		a[i,j] = 0
	}
}

a[2*sample(1:rown,1),1] = 0
a[2*sample(1:coln,1),ncol(a)] = 0

posx = 1
posy = 1

check = TRUE
path = c()

while(check){
	path = rbind(c(posx,posy),path)
	a[2*posx,2*posy] = 1
	sur = checkSurrounding(posx,posy,a)
	
	if(length(sur)>0){
		s = sample(sur,1)
		if(s=="l"){
			posx = posx - 1
			a[2*posx+1,2*posy] = 0
		}else if(s=="r"){
			posx = posx + 1
			a[2*posx-1,2*posy] = 0
		}else if(s=="u"){
			posy = posy - 1
			a[2*posx,2*posy+1] = 0
		}else if(s=="d"){
			posy = posy + 1
			a[2*posx,2*posy-1] = 0
		}
		
	}else{
		
		path = rbind(path[-1,])
		
		if(length(path)==0){
			check = FALSE
		}else{
			posx = path[1,1]
			posy = path[1,2]
			path = rbind(path[-1,])
		}
	}
}

##pdf("C:/Users/murat/Desktop/maze.pdf")
plot(0,xlim=c(1,ncol(a)),ylim=c(1,nrow(a)),col="white")

for(i in seq(2,nrow(a),by=2)){
	for(j in seq(2,ncol(a),by=2)){
		if(a[i-1,j] == 1){
			lines(c(j-1,j+1),c(i-1,i-1))
		}
		if(a[i+1,j] == 1){
			lines(c(j-1,j+1),c(i+1,i+1))
		}
		if(a[i,j-1] == 1){
			lines(c(j-1,j-1),c(i-1,i+1))
		}
		if(a[i,j+1] == 1){
			lines(c(j+1,j+1),c(i-1,i+1))
		}
	}
}
##dev.off()

