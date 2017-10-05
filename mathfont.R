library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  

library(ggplot2)
data(mtcars)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +     
  ggtitle(expression(y==alpha+beta*x)) +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme_bw() +
  theme(text=element_text(family="Symbol", face="bold", size=12)) #Times New Roman, 12pt, Bold
#example taken from the Github project page

font_install("fontcm")
loadfonts()
pdf("plot_cm.pdf", family="CM Roman", width=5.5, height=5)

curve(dnorm, from=-3, to=3, main="Normal Distribution")
text(x=0, y=0.1, cex=1.5, expression(italic(y == frac(1, sqrt(2 * pi)) *
                                     e ^ {-frac(x^2, 2)} )))

dev.off()
embed_fonts("plot_cm.pdf", outfile="plot_cm_embed.pdf")

library(ggplot2)
library(extrafont)

loadfonts()

pdf("ggplot_cm.pdf", width=4, height=4)
p <- qplot(c(1,5), c(1,5)) +
  xlab("Made with CM fonts") + ylab("Made with CM fonts") +
  ggtitle("Made with CM fonts")

# Equation
eq <- "italic(sum(frac(1, n*'!'), n==0, infinity) ==
       lim(bgroup('(', 1 + frac(1, n), ')')^n, n %->% infinity))"

# Without the new fonts
p + annotate("text", x=3, y=3, parse=TRUE, label=eq)

# With the new fonts
p + annotate("text", x=3, y=3, parse=TRUE, family="CM Roman", label=eq) +
    theme(text         = element_text(size=16, family="CM Roman"),
          axis.title.x = element_text(face="italic"),
          axis.title.y = element_text(face="bold"))

dev.off()

# Embed the fonts
embed_fonts("ggplot_cm.pdf", outfile="ggplot_cm_embed.pdf")
