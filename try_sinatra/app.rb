require 'sinatra'
require 'sinatra/reloader'
require 'active_record'

ActiveRecord::Base.establish_connection(
  adapter: "mysql2",
  host: "localhost",
  username: "root",
  password: "password",
  database: "sinatra",
)

class User < ActiveRecord::Base
 
end


get '/' do
  erb :root
end

post '/new' do 
  if params[:name] and params[:gender] and params[:pref_name]
    User.create(name: params[:name], gender: params[:gender], pref: params[:pref_name])
    '成功しました'
  else
    '失敗しました'
  end
end
