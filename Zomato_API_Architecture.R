################## Zomato API Documentation - Basic Architecture #####################
rm(list = ls())
options(scipen = 999)

# libraries:
library(httr)
library(jsonlite)

# Zomato Authentication Key:
AUTH_Key = "f450$$%7af5e%$##@f39034a07$##@@@&f"

# Documentation : Common Services
# /categories 
categories = function(Key){
  req = GET(url = "https://developers.zomato.com/api/v2.1/categories?",
            add_headers(User_key = Key))
  package = data.frame(fromJSON(rawToChar(req$content)))
  return(package)
  
}
categories(AUTH_Key)

# /cities
## key : Zomato Authentication Key --> type :character
## city_name : City                --> type : character
## latitude : Latitude             --> type : double
## longitude : Longitude           --> type : double
## count : Max results to display  --> type : integer
cities = function(key,city_name,latitude,longitude,count){
  req = GET(url = "https://developers.zomato.com/api/v2.1/cities?",
            add_headers(User_key = key),
            query = list(q = city_name,
                         lat = latitude,
                         lon = longitude,
                         count = count))
  package = data.frame(fromJSON(rawToChar(req$content)))
  return(package)
}
cities(AUTH_Key,"Kolkata",22.572645,88.363892,10)

# /collections
## key : Zomato Authentication Key        --> type : character
## city_id : City ID from cities function --> type : integer
## latutide : Latitude                    --> type : double
## longitude : Longitude                  --> type : double
## count : Max results to display         --> type : integer
collections = function(key,city_id,latitude,longitude,count){
  req = GET("https://developers.zomato.com/api/v2.1/collections?",
            add_headers(User_key = key),
            query = list(city_id = city_id,
                         lat = latitude,
                         lon = longitude,
                         count = count))
  package = data.frame(fromJSON(rawToChar(req$content)))
  return(package)
}
collections(AUTH_Key,2,22.572645,88.363892,10)

# /cuisines
## key : Zomato Authentication Key  --> type : character
## city_id : City ID from cities    --> type : integer
## lat : Latitude                   --> type : double
## lon : Longitude                  --> type : double
cuisines = function(key,city_id,latitude,longitude){
  req = GET("https://developers.zomato.com/api/v2.1/cuisines?",
            add_headers(User_key = key),
            query = list(city_id = city_id,
                         lat = latitude,
                         lon = longitude))
  package = data.frame(fromJSON(rawToChar(req$content)))
  return(package)
}
cuisines(AUTH_Key,2,22.572645,88.363892)

# /establishments 
## key : Zomato Authentication Key        --> type : character
## city_id : City ID from cities function --> type : integer
## latutide : Latitude                    --> type : double
## longitude : Longitude                  --> type : double
establishments = function(key,city_id,latitude,longitude){
  req = GET("https://developers.zomato.com/api/v2.1/establishments?",
            add_headers(User_key = key),
            query = list(city_id = city_id,
                         lat = latitude,
                         lon = longitude))
  package = data.frame(fromJSON(rawToChar(req$content)))
  return(package)
}
establishments(AUTH_Key,2,22.572645,88.363892)

# /geocode
## key       : Zomato Authentication Key --> type : character
## latitude  : Latitude                  --> type : double
## longitude : Longitude                 --> type : double
geocode = function(key,latitude,longitude){
  req = GET("https://developers.zomato.com/api/v2.1/geocode?",
            add_headers(User_key = key),
            query = list(lat = latitude,
                         lon = longitude))
  load = fromJSON(rawToChar(req$content))
  package = cbind.data.frame(data.frame(load$location),
                             data.frame(t(unlist(load$popularity))))
  nearby = load$nearby_restaurants
  return(package)
}
geocode(AUTH_Key,22.572645,88.363892)

# Documentation : Location Based Services

# /location
## key : Zomato Authentication Key        --> type : character
## location : Name of the City            --> type : character
## latitude : Latitude                    --> type : double
## longitude : Longitude                  --> type : double
## count : Max number of results to fetch --> type : integer
location = function(key,location,latitude,longitude,count){
  req = GET("https://developers.zomato.com/api/v2.1/locations?",
            add_headers(User_key = key),
            query = list(query = location,
                         lat = latitude,
                         lon = longitude,
                         count = count))
  package = data.frame(fromJSON(rawToChar(req$content)))
  return(package)
}
location(AUTH_Key,"Kolkata",22.572645,88.363892,10)

# /location_details 
## key : Zomato Authentication Key                       --> type : character
## entityid : location ID obtained from location API     --> type: integer
## entitytype : location type obtained from location API --> type: character
location_details = function(key,entityid,entitytype){
  req = GET("https://developers.zomato.com/api/v2.1/location_details?",
            add_headers(User_key = key),
            query = list(entity_id = entityid,
                         entity_type = entitytype))
  
  load = fromJSON(rawToChar(req$content))
  package = cbind.data.frame(data.frame(popularity = load$popularity,
                                        nightlife_index = load$nightlife_index,
                                        nearby_res = paste(load$nearby_res,collapse = "-"),
                                        top_cuisines = paste(load$top_cuisines,collapse = "-"),
                                        popularity_res = load$popularity_res,
                                        nightlife_res = load$nightlife_res,
                                        subzone = load$subzone,
                                        subzone_id = load$subzone_id,
                                        city = load$city
                                        ),
                             data.frame(t(unlist(load$location))))
  return(package)
}
location_details(AUTH_Key,671,"group")

# Documentation : real-time restaurant services :

# /dailymenu
## key : Zomato Authentication Key        --> type : character
## rest_id : Restaurant ID                --> type : integer
daily_menu = function(key,rest_id){
  req = GET("https://developers.zomato.com/api/v2.1/dailymenu?",
            add_headers(User_key = key),
            query = list(res_id = rest_id))
  load = fromJSON(rawToChar(req$content))
  package = data.frame(load$daily_menus)
  return(package)
} 
daily_menu(AUTH_Key,16507624)

# /restaurant
## key : Zomato Authentication Key        --> type : character
## rest_id : Restaurant ID                --> type : integer
restaurant = function(key,rest_id){
  req = GET("https://developers.zomato.com/api/v2.1/restaurant?",
            add_headers(User_key = key),
            query = list(res_id = rest_id))
  
  load = fromJSON(rawToChar(req$content))
  package = data.frame(restaurant = load$name,
                       establishment_type = load$establishment,
                       timing = load$timings,
                       cuisines = load$cuisines,
                       highlights = paste(load$highlights,collapse = "-"),
                       currency = load$currency,
                       review_count = load$all_reviews_count,
                       photo_count = load$photo_count,
                       data.frame(load$location),
                       data.frame(load$user_rating))
  return(package)
}
restaurant(AUTH_Key,16774318)

# /reviews
## key : Zomato Authentication Key                 --> type : character
## rest_id : Restaurant ID                         --> type : integer
## start_offset : fetch results after this offset  --> type :integer
reviews = function(key,rest_id,start_offset,count){
  req = GET("https://developers.zomato.com/api/v2.1/reviews?",
            add_headers(User_key = key),
            query = list(res_id = rest_id,
                         start = start_offset,
                         count = count))
  
  load = fromJSON(rawToChar(req$content))
  package = data.frame(load$user_reviews$review)
  return(package)
}
reviews(AUTH_Key,16774318,10,20)

# /search
search = function(key,
                  entityid,
                  entitytype,
                  keyword,
                  start_offset,
                  count,
                  latitude,
                  longitude,
                  radius,
                  cuisines_list,
                  establishmenttype,
                  collectionid,
                  category,
                  sort,
                  order){
  
  req = GET("https://developers.zomato.com/api/v2.1/search?",
            add_headers(User_key = key),
            query = list(entity_id = entityid,
                         entity_type = entitytype,
                         q = keyword,
                         start = start_offset,
                         count = count,
                         lat = latitude,
                         lon = longitude,
                         radius = radius,
                         cuisines = cuisines_list,
                         establishment_type = establishmenttype,
                         collection_id = collectionid,
                         category = category,
                         sort = sort,
                         order = order))
  
  load = fromJSON(rawToChar(req$content))
  package = data.frame(load$restaurants)
  return(package)
}

