require 'active_record'

ActiveRecord::Base.establish_connection(
    "adapter" =>"sqlite3",
    "database" => "./blog.db"
)

class Movie < ActiveRecord::Base #import.sqlのテーブル名postsを大文字、単数形に変える
end
#これでARとRubyのオブジェクトが紐付けられた

movie=Movie.create
movie.title='博士の異常な愛情'

p movie.title
