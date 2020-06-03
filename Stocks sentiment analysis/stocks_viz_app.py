###Data visualisation : (please use the date range slider  first so the other buttons can work)
###(you can bring it back to full range afterwards if you wish to do so)

##importing libraries
from math import pi
from random import randint
import pandas as pd
import json
from datetime import datetime
from bokeh.plotting import figure
from bokeh.models import *
from bokeh.io import curdoc, show
from bokeh.layouts import row, column
from bokeh.transform import factor_cmap, cumsum
from bokeh.palettes import Spectral6, Category20_16, Category20c

##creating needed variables :
codes = {'Apple': 'AAL', 'Google': 'GOOGL','Microsoft': 'MSFT'}
df = pd.read_csv('datasets/all_stocks_5yr.csv')
df['date'] = pd.to_datetime(df['date'])
dat_colors = Category20_16
sa_df=pd.read_csv('sentiment_analysis/results.csv')
high_low=[0,1]

##data creation functions for different plots :
def make_histdata(range_start, range_end):
	"""Create a ColumnDataSource based on changed variables for the bar chart plot"""
	X = list(codes.keys())
	subset = df[df['date'].between(range_start,range_end)]
	Y = [subset[subset.Name == company_code].volume.sum() for company_code in list(codes.values())]

	return ColumnDataSource(data={'x': X, 'y': Y})

def make_dataset( range_start, range_end,high_low,company_name):
	"""Create a ColumnDataSource based on changed variables for the lines plot"""
	company_code = codes[company_name]
	X = []
	Y = []
	colors = []
	legend_group = []

	subset = df[df.Name == company_code].drop('Name',axis=1)

	subset = subset[subset['date'].between(range_start,range_end)]

	x = subset['date'].tolist()
	y1 = subset['high'].tolist()
	y2 = subset['low'].tolist()

	if high_low.count(1) > 0 :
		X.append(x)
		Y.append(y2)
		colors.append('green')
		legend_group.append('low')
	if high_low.count(0) > 0 :
		X.append(x)
		Y.append(y1)
		colors.append('red')
		legend_group.append('high')
	new_src = ColumnDataSource(data={'x': X, 'y': Y, 'color': colors, 'label': legend_group})

	return new_src

def make_piedata(company) :
	"""Create a ColumnDataSource based on changed variables for the pie sentiment plot """
	df = sa_df[sa_df['Company Name'] == company].drop(columns=['Company Name'])
	data = df.to_dict('split')
	del data['index']
	data['data'] = data['data'][0]
	data['angle'] = [data['data'][i]* 2 * pi/100 for i in range(len(data['data']))]
	data['color'] = Category20c[len(data['data'])]

	return ColumnDataSource(data = data)

def make_plot(src):
	"""Create a Plot figure based on changed data for the bar lines plot"""
	p = figure(plot_width = 600, plot_height = 600,
			   title = 'High & Low stocks per company :',
			   x_axis_label = 'Dates', y_axis_label = 'Stock value')


	p.multi_line('x', 'y', color = 'color', legend = 'label', 
				 line_width = 3,
				 source = src)

	# hiver tools
	hover = HoverTool(tooltips=[('high/low', '@label'), 
								('Date', '$x'),
								('price', '$y')],
					  line_policy = 'next')
	p.add_tools(hover)

	#style
		#main title
	p.title.align = 'center'
	p.title.text_font_size = '20pt'
	p.title.text_font = 'serif'

		# Axis titles
	p.xaxis.axis_label_text_font_size = '14pt'
	p.xaxis.axis_label_text_font_style = 'bold'
	p.yaxis.axis_label_text_font_size = '14pt'
	p.yaxis.axis_label_text_font_style = 'bold'

		# Tick labels
	p.xaxis.major_label_text_font_size = '12pt'
	p.yaxis.major_label_text_font_size = '12pt'

	return p

def make_pieplot(pie_src):

	p = figure(plot_height=350, title="Sentiment analysis", toolbar_location=None,
	           tools="hover", tooltips="@columns : @data %", x_range=(-0.5, 1.0))

	p.wedge(x=0, y=1, radius=0.4,
	        start_angle=cumsum('angle', include_zero=True), end_angle=cumsum('angle'),
	        line_color="white", fill_color='color',legend = 'columns' , source=pie_src)	

	#style
	p.axis.axis_label=None
	p.axis.visible=False
	p.grid.grid_line_color = None
	p.title.align = 'center'
	p.title.text_font_size = '20pt'
	p.title.text_font = 'serif'

	return p

def make_histplot(hist_src):

	p = figure(x_range=hist_src.data['x'], plot_height=200, title="comparisons",
		toolbar_location=None, tools="")

	p.vbar(x='x', top='y', width=0.9, source=hist_src)

	#style
	p.title.text_font_size = '20pt'
	p.title.text_font = 'serif'

	return p

##Update function
def update(attr, old, new):
	"""The update fuction used to update our data based on user choices"""
	high_low_n=list(prices_check.active)
    #creating new data sources 
	new_src = make_dataset(range_start = datetime.fromtimestamp(prices_date.value[0]/1000),
								range_end = datetime.fromtimestamp(prices_date.value[1]/1000),
								high_low=high_low_n,
								company_name=str(prices_select.value))
	new_pie_src = make_piedata(company=prices_select.value)
	new_hist_src = make_histdata(range_start = datetime.fromtimestamp(prices_date.value[0]/1000),
								range_end = datetime.fromtimestamp(prices_date.value[1]/1000))

	#changing the main data sources 
	src.data.update(new_src.data)
	pie_src.data.update(new_pie_src.data)
	hist_src.data.update(new_hist_src.data)


##Widgets creation :
#title
mainTitle = Div(text="<font size='6' color='darkblue'><b>Stocks and sentiment evaluation : </b></font>", sizing_mode="stretch_width")

#separator
sep = Div(text="<font size='6' color='lightgrey'>{0} </font>".format('-'*125), sizing_mode="stretch_width")

#buttons and change triggers
prices_check = CheckboxButtonGroup(labels=['high', 'low'], 
									   active = [0, 1])
prices_check.on_change('active', update)
prices_select = Select(title="Companies :", value="Apple", options=['Apple', 'Microsoft','Google'])
prices_select.on_change('value', update)
prices_date = DateRangeSlider(start = min(df['date']), end = max(df['date']), value = (min(df['date']), max(df['date'])),
						   step = 5, title = 'Date Range between : ')
prices_date.on_change('value', update)

#initial plots, data sources and layouts
#for the stocks 
src = make_dataset(range_start = prices_date.value[0],
						range_end = prices_date.value[1],
						high_low = high_low,
						company_name= 'Apple')
prices_fig = make_plot(src)
prices_layout = row(column(prices_select,prices_check,prices_date),prices_fig)

#for the sentiment analysis and volume count 
pie_src = make_piedata('Apple')
sentiment_fig = make_pieplot(pie_src)
hist_src = make_histdata(range_start = prices_date.value[0],
						range_end = prices_date.value[1])
volume_hist = make_histplot(hist_src)
sv_layout = column(sentiment_fig,volume_hist)

#creating the main layout
layout = column(mainTitle,sep,row(prices_layout,sv_layout),sizing_mode='stretch_both')

#adding the layout to server
curdoc().add_root(layout)





































#show(layout)

