# one-way-anova
The R function for doing one-way anova
mcompare<-function(data){###########doing multiple comparison based on one-way anova
    library(agricolae)
    output<-data.frame(component=character(0),trt=character(0),
                       means=numeric(0),
                       M=character(0),stringsAsFactors=F)
    for(j in 2:length(data)){
      
      y<-length(levels(data[,1]))
      
        result.aov<-aov(data[,j]~data[,1],data=data)
        ifelse(var(data[,j])==0,
               duncan<-data.frame(aggregate(data[,j],data[1],mean),
                                     rep("a",y)),
               
               #result<-summary(result.aov)
               #lsd<-LSD.test(result.aov,names(result.aov$model)[2]) #LSD#
               #hsd<-HSD.test(result.aov,names(result.aov$model)[2]) #HSD#
               #snk<-SNK.test(result.aov,names(result.aov$model)[2]) #SNK#
               duncan<-data.frame(rownames(data.frame(duncan.test(result.aov,
                                              names(result.aov$model)[2],
                                              alpha=0.05)$groups)),
                                  data.frame(duncan.test(result.aov,
                                             names(result.aov$model)[2],
                                             alpha=0.05)$groups)
                                  )         
               
               )
        
        x<-colnames(data[j])
        binddata<-data.frame(rep(x,y),stringsAsFactors=F)
        duncan[,2]<-as.character(duncan[,2])
        duncan<-cbind(binddata,duncan)
        colnames(duncan)<-c("Component","trt","mean","M")
        output<-rbind(output,duncan)
               
        
    } 
    colnames(output)<-c("component","trt","means","M")
    output$component<-factor(output$component,order=T,levels=colnames(data[-1]))
    output$trt<-as.factor(output$trt)
    output<-output[order(output$component,output$trt),]    
}
