library(shiny)
library(editbl)

ui <- fluidPage(
		tags$h1('editbl - demo'),
		br(),
		editbl::eDTOutput('data'),
		verbatimTextOutput('modifiedData'),
		helpText('Note different behavior of different data types.'),
		helpText('Edit cells by clicking the in-row buttons or directly within the table.'),
		helpText("See how 'id' is generated on the fly."),
		helpText("Note how only 'city_id' is stored even if not displayed. Cities are a separate table with foreign key."),
)
		
server <- function(input,output,session){
	superheroes <- data.frame(
			id = sapply(1:3, uuid::UUIDgenerate),
			first_name = c('Bruce', 'Tony', 'Clark'),
			last_name = c('Wayne', 'Stark', 'Kent'),
			first_appearance = as.Date(c('1939-03-30', '1962-03-15', '1938-04-18')),
			publisher = as.factor(c('DC Comics', 'Marvel', 'DC Comics')),
			cars_owned = c(5,17,1),
			city_id = c(1,2,3)
	)

	cities <- data.frame(
		  city_id = c(1,2,3),
		  name = c('New York', 'Gotham City', 'Smallville'),
		  country = c('US', 'US', 'US')
	)
		
	
	modifiedData <- editbl::eDT(
			id = 'data',
			data = superheroes,
			foreignTbls = list(foreignTbl(superheroes, cities, 'city_id')),
			options = list(columnDefs = list(list(visible=FALSE, targets=c("id","city_id")))),
			defaults = dplyr::tibble(id = uuid::UUIDgenerate())
	)
	
	output$modifiedData <- renderPrint({
				data <- modifiedData$state()
				print(str(data))
				print(data)
			}
	)
	
}
shiny::shinyApp(ui,server)

