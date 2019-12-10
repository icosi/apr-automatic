# --------------------------------------------------------------------------------------------------
#
#	Plots of 7-edge digits
#
# --------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------
#
#	Coordinates of edge endpoints
#
# --------------------------------------------------------------------------------------------------
	A<-c(-1,2)
	B<-c(1,2)
	C<-c(-1,0)
	D<-c(1,0)
	E<-c(-1,-2)
	F<-c(1,-2)
# --------------------------------------------------------------------------------------------------
#
#	edge
#
#	Auxiliary function. 
#	Plots a single horizontal or vertical edge in a "seven edge" digit by joining the
#	(x1,y1) and (x2,y2) endpoints. 
#	For a horizontal edge (x1,y1) is the leftmost endpoint.
#	For a vertical edge (x1,y1) is the lower endpoint.
#
#	There are seven possible edges, numbered 1 to 7. 
#
#			S1
#
#	S2				S3
#
#			S4
#
#	S5				S6
#
#			S7
#
#	When the integer optional parameter 'seg.num' value is a number from 1 to 7 this number is
#	written as a label on the edge center; for any other value no label is written.
#
#	eps is a small number, intended to avoid, for esthetic reasons, a closed polygon.
#
# --------------------------------------------------------------------------------------------------
	eps<-0.1
	edge<-function(x1,y1,x2,y2,edge.num=0){
		if(y1==y2){
			x1<-x1+eps;x2<-x2-eps
			}
		else{
			y1<-y1+eps;y2<-y2-eps
			}
		segments(x1,y1,x2,y2,lwd=11,col="blue")
		if (edge.num>0&edge.num<8){
			xc<-(x1+x2)/2
			yc<-(y1+y2)/2
			points(xc,yc,pch=19,col="white",cex=4.5)
			points(xc,yc,pch=21,col="blue",cex=4.5,lwd=2)
			text(xc,yc,edge.num,col="blue",cex=1.5)
			}
		}
# --------------------------------------------------------------------------------------------------
#
#	edge.1 to edge.7
#
#	Auxiliary functions. 
#
#	Plot each of the seven possible edges.
#
#	The optional Boolean 'numbered' 
#
# --------------------------------------------------------------------------------------------------	
	
	edge.1<-function(numbered=FALSE){
		edge(A[1],A[2],B[1],B[2],edge.num=ifelse(numbered,1,0))
		}
	
	edge.2<-function(numbered=FALSE){
		edge(C[1],C[2],A[1],A[2],edge.num=ifelse(numbered,2,0))
		}
		
	edge.3<-function(numbered=FALSE){
		edge(D[1],D[2],B[1],B[2],edge.num=ifelse(numbered,3,0))
		}
	
	edge.4<-function(numbered=FALSE){
		edge(C[1],C[2],D[1],D[2],edge.num=ifelse(numbered,4,0))
		}
		
	edge.5<-function(numbered=FALSE){
		edge(E[1],E[2],C[1],C[2],edge.num=ifelse(numbered,5,0))
		}
		
	edge.6<-function(numbered=FALSE){
		edge(F[1],F[2],D[1],D[2],edge.num=ifelse(numbered,6,0))
		}
		
	
	edge.7<-function(numbered=FALSE){
		edge(E[1],E[2],F[1],F[2],edge.num=ifelse(numbered,7,0))
		}
	
	edge.array<-list(edge.1,edge.2,edge.3,edge.4,edge.5,edge.6,edge.7)

# --------------------------------------------------------------------------------------------------
#
#	digits.m
#
# A [10,7] matrix of zeros and ones.
#	Each row 'j' has '1's and '0's to set 'ON' or 'OFF' the required 
# edges to plot digit 'j' ('0' is the tenth one).
#
# --------------------------------------------------------------------------------------------------
	digits.m<-matrix(as.logical(c(
	c(0,1,1,0,1,1,1,1,1,1),
	c(0,0,0,1,1,1,0,1,1,1),
	c(1,1,1,1,0,0,1,1,1,1),
	c(0,1,1,1,1,1,0,1,1,0),
	c(0,1,0,0,0,1,0,1,0,1),
	c(1,0,1,1,1,1,1,1,1,1),
	c(0,1,1,0,1,1,0,1,1,1))),nrow=10)
	dimnames(digits.m)<-list(D=sprintf("%d",c(1:9,0)), S=sprintf("x%d",1:7))
# --------------------------------------------------------------------------------------------------
#
#	Plot a digit stored in a given row 'j' in the 'digits.m' matrix.
#
# --------------------------------------------------------------------------------------------------
plot.digit<-function(j){
	plot(c(0,0),type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),axes=FALSE,xlab="",ylab="",frame=TRUE)	
	for (i in subset(edge.array,digits.m[j,])) i()
}
# --------------------------------------------------------------------------------------------------
#
#	Plot a digit stored as a vector 'v' with seven entries '0'/'1'.
#
# --------------------------------------------------------------------------------------------------
	plot.faulty.digit<-function(v){
	  plot(c(0,0),type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),axes=FALSE,xlab="",ylab="",frame=TRUE)	
	  for (i in subset(edge.array,v)) i()
	}
# --------------------------------------------------------------------------------------------------
#
#	Plot all digits
#
# --------------------------------------------------------------------------------------------------
show.all.digits<-function(){
	old.par<-par(mfrow=c(2,5),mai=c(0,0,0,0))
	for (i in 1:10) plot.digit(i)
	par(old.par)
	}
# --------------------------------------------------------------------------------------------------
#
#	Plot edges matrix with numbered edges
#
# --------------------------------------------------------------------------------------------------
plot.digits.matrix.with.segment.indexes<-function(...){
	plot(c(0,0),type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),axes=FALSE,
	      xlab="",ylab="",frame=TRUE,...)
	for (i in edge.array) i(1)
	}
