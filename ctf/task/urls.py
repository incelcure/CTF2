from django.urls import path, re_path
from django.views.decorators.cache import cache_page

from .views import *

urlpatterns = [
    path('tasks/', task_list_view, name='task_list'),
    path('task/<int:task_id>', task_detail_view, name='task_detail'),
]
