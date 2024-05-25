from django.shortcuts import render
from django.contrib.auth import logout, login
from django.contrib.auth.views import LoginView
from django.core.paginator import Paginator
from django.http import HttpResponse, HttpResponseNotFound, Http404
from django.shortcuts import render, redirect, get_object_or_404
from django.urls import reverse_lazy
from django.views.generic import ListView, DetailView, CreateView, FormView
from django.contrib.auth.mixins import LoginRequiredMixin

from .models import *


def task_list_view(request):
    tasks = Task.objects.all()
    return render(request, 'task/task_list.html', {'tasks': tasks})


def task_detail_view(request, task_id):
    task = get_object_or_404(Task, id=task_id)
    if request.method == 'POST':
        user_answer = request.POST.get('answer')
        if user_answer == task.answer:
            return HttpResponse('Correct answer!')
        else:
            return HttpResponse('Incorrect answer.')
    return render(request, 'task/task_detail.html', {'task': task})


def rules_view(request):
    return render(request, 'task/rules.html')
