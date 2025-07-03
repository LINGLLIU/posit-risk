# app.R
library(shiny)
library(shinyWidgets)
library(plotly)
library(catboost)

# 确认文件存在
if(!file.exists("prism_catboost_model.cbm")) stop("模型文件未找到")
if(!file.exists("feature_info.rds")) stop("特征信息文件未找到")

model <- catboost.load_model("prism_catboost_model.cbm")
feature_info <- readRDS("feature_info.rds")
# 固定特征顺序
feature_order <- c("Gender", "Age2", "BMI20cut", "Smoking", "Childhoodcough",
                   "Cough", "Sputum", "Wheeze", "Dyspnea", "Familyhistory",
                   "TB", "Asthma", "HBP", "DM")


# UI界面保持不变
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$style(HTML("
      .risk-high { color: #440154; font-weight: bold; }
      .risk-low { color: #FDE725; font-weight: bold; }
      .well-panel { 
        background-color: #f8f9fa; 
        border-radius: 10px; 
        padding: 15px; 
        margin-bottom: 15px; 
      }
      .result-panel {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        margin-top: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  titlePanel("PRISm Risk Prediction Tool"), 
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "well-panel",
          h4("Demographics"),
          pickerInput("Gender", "Gender", choices = c("Female", "Male")),
          pickerInput("Age2", "Age", choices = c("<63 years", "≥63 years"))
      ),
      
      div(class = "well-panel",
          h4("Physical Measures"),
          pickerInput("BMI20cut", "BMI", choices = c("≥20 kg/m²", "<20 kg/m²"))
      ),
      
      div(class = "well-panel",
          h4("Respiratory Symptoms"),
          pickerInput("Smoking", "Smoking history", choices = c("No", "Yes")),
          pickerInput("Cough", "Cough", choices = c("No", "Yes")),
          pickerInput("Sputum", "Sputum", choices = c("No", "Yes")),
          pickerInput("Wheeze", "Wheezing", choices = c("No", "Yes")),
          pickerInput("Dyspnea", "Dyspnea", choices = c("No", "Yes"))
      ),
      
      div(class = "well-panel",
          h4("Medical History"),
          pickerInput("Childhoodcough", "Childhood Cough", choices = c("No", "Yes")),
          pickerInput("Familyhistory", "Family History", choices = c("No", "Yes")),
          pickerInput("TB", "TB History", choices = c("No", "Yes")),
          pickerInput("Asthma", "Asthma", choices = c("No", "Yes")),
          pickerInput("HBP", "Hypertension", choices = c("No", "Yes")),
          pickerInput("DM", "Diabetes", choices = c("No", "Yes"))
      ),
      
      actionBttn("calculate", "Calculate Risk", 
                 style = "fill", color = "primary", size = "lg", block = TRUE)
    ),
    
    mainPanel(
      width = 8,
      div(class = "result-panel",
          h3("PRISm Risk Assessment"),
          uiOutput("risk_output"),
          plotlyOutput("risk_gauge")
      ),
      
      div(class = "disclaimer",
          p("Note: This tool provides risk estimates only and does not constitute medical advice.")
      )
    )
  )
)

# Server逻辑 - 最终修正版
server <- function(input, output, session) {
  # 加载模型
  model <- catboost.load_model("prism_catboost_model.cbm")
  
  # 创建响应式数据准备函数
  prepare_data <- reactive({
    req(input$calculate)
    
    # 1. 创建数据框（确保列名正确）
    data <- data.frame(
      Gender = as.numeric(input$Gender == "Male"),
      Age2 = as.numeric(input$Age2 == "≥63 years"),
      BMI20cut = as.numeric(input$BMI20cut == "<20 kg/m²"),
      Smoking = as.numeric(input$Smoking == "Yes"),
      Childhoodcough = as.numeric(input$Childhoodcough == "Yes"),
      Cough = as.numeric(input$Cough == "Yes"),
      Sputum = as.numeric(input$Sputum == "Yes"),
      Wheeze = as.numeric(input$Wheeze == "Yes"),
      Dyspnea = as.numeric(input$Dyspnea == "Yes"),
      Familyhistory = as.numeric(input$Familyhistory == "Yes"),
      TB = as.numeric(input$TB == "Yes"),
      Asthma = as.numeric(input$Asthma == "Yes"),
      HBP = as.numeric(input$HBP == "Yes"),
      DM = as.numeric(input$DM == "Yes")
    )
    
    # 2. 强制特征顺序
    data <- data[, feature_order, drop = FALSE]
    return(data)
  })
  
  # 风险计算核心函数
  calculate_risk <- reactive({
    data <- prepare_data()
    
    # 3. 转换为矩阵并验证
    mat <- as.matrix(data)
    cat("Prediction matrix:\n")
    print(mat)
    
    # 4. 创建Pool并预测
    pool <- catboost.load_pool(mat)
    prediction <- catboost.predict(model, pool, prediction_type = "Probability")
    cat("Raw prediction:", prediction, "\n")
    
    # 5. 提取阳性类概率（PRISm=1的概率）
    risk_score <- round(prediction[1] * 100, 1)  # 注意：根据您的输出，prediction[1]是阳性概率
    risk_level <- ifelse(risk_score >= 50, "High Risk", "Low Risk")
    
    return(list(score = risk_score, level = risk_level))
  })
  
  # 风险仪表盘
  output$risk_gauge <- renderPlotly({
    risk <- calculate_risk()
    
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = risk$score,
      title = list(text = "Risk Level"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "grey"),
        steps = list(
          list(range = c(0, 50), color = "#FDE725"),
          list(range = c(50, 100), color = "#440154")
        ),
        threshold = list(
          line = list(color = "grey"),
          
          value = risk$score)
      )
    ) %>% layout(margin = list(l=20, r=30))
  })
  
  # 风险结果输出
  output$risk_output <- renderUI({
    risk <- calculate_risk()
    
    div(class = ifelse(risk$level == "High Risk", "risk-high", "risk-low"),
        h2(paste0("PRISm Risk Score: ", risk$score, "%")),
        h3(paste0("Risk Classification: ", risk$level))
    )
  })
}

# 运行应用
shinyApp(ui = ui, server = server)

