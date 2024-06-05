from django.urls import path, re_path
from django.views.decorators.cache import cache_page

from .views import *

urlpatterns = [
    path('', rules_view, name='index'),
    path('tasks/', task_list_view, name='task_list'),
    path('task/<int:task_id>', task_detail_view, name='task_detail'),
    path('rules/', rules_view, name='rules'),
    path('register/', register_view, name='register'),
    path('login/', login_view, name='login'),
    path('logout/', logout_view, name='logout'),
    path('leaderboard/', leaderboard_view, name='leaderboard'),
    path('profile/', profile_view, name='profile'),
    path('profile/edit/', edit_profile_view, name='edit_profile'),
]
