class Api::V1::Auth::RegistrationsController < DeviseTokenAuth::RegistrationsController

  private
  # ユーザー登録時に許可する項目
  def sign_up_params
    params.permit(:name, :email, :password, :password_confirmation)
  end
　
  #ユーザーが更新可能な項目
  def account_update_params
    params.permit(:name, :email,:nickname,:image)
  end
end