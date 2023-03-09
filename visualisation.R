library(readr)
library(reshape2)


df <- read_csv('WA_Fn-UseC_-HR-Employee-Attrition.csv')
head(df)

dim(df)

summary(df)

colSums(is.na(df))

attrition_count <- data.frame(table(df$Attrition))
attrition_count

library(ggplot2)
attrition_count$Freq <- attrition_count$Freq / sum(attrition_count$Freq)
ggplot(data = attrition_count, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(fill = "Attrition") +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("blue", "orange")) +
  ggtitle("Attrition Pie Chart")


df <- df[, !names(df) %in% c("EmployeeCount", "EmployeeNumber")]
attrition_dummies <- as.data.frame(model.matrix(~Attrition - 1, data = df))

head(attrition_dummies)

df <- cbind(df, attrition_dummies)
df <- df[, !names(df) %in% c("Attrition", "No")]
head(df)


ggplot(df, aes(x = Gender, y = Yes)) +
  geom_bar(stat = "identity") +
  labs(title = "Attrition by Gender")

ggplot(df, aes(x = Department, y = Yes)) +
  geom_bar(stat = "identity") +
  labs(title = "Attrition by Department")

ggplot(df, aes(x = BusinessTravel, y = Yes)) +
  geom_bar(stat = "identity") +
  labs(title = "Attrition by Business Travel")

# ggplot(melt(cor(df)), aes(Var1, Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1)) +
#   coord_fixed() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   ggtitle("Correlation Heatmap")
