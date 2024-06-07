import json

from django.contrib.auth.decorators import login_required
from django.contrib.auth.forms import AuthenticationForm
import requests
from django.db.models import Sum
from django.contrib.auth import logout, login, authenticate
from django.shortcuts import render, redirect, get_object_or_404
from django.contrib import messages
from django.contrib.sessions.models import Session
import pickle

from .forms import UserRegisterForm
from .models import *
from .metrics import registration_counter, login_counter, attempt_counter
# from .utils import TokenCache
from .utils import get_user_token

from .forms import ProfileForm


def register_view(request):
    if request.method == 'POST':
        form = UserRegisterForm(request.POST)
        if form.is_valid():
            form.save()
            registration_counter.inc()
            username = form.cleaned_data.get('username')
            messages.success(request, f'Account created for {username}!')
            return redirect('login')
    else:
        form = UserRegisterForm()
    return render(request, 'task/register.html', {'form': form})


def login_view(request):
    if request.method == 'POST':
        form = AuthenticationForm(request, data=request.POST)
        if form.is_valid():
            username = form.cleaned_data.get('username')
            password = form.cleaned_data.get('password')
            user = authenticate(username=username, password=password)
            if user is not None:
                login(request, user)
                login_counter.inc()

                completed_attempts = Attempt.objects.filter(user=request.user, correct=True).select_related('task')
                total_points = completed_attempts.aggregate(Sum('task__points'))['task__points__sum'] or 0
                casino_set_json = {'secret': settings.SECRET_KEY, 'subj': user.username, 'value': total_points}
                casino_set_url = f"http://{settings.CASINO_HOST}/api/set"
                response = requests.post(casino_set_url, json=casino_set_json)
                response.raise_for_status()

                return redirect('task_list')
            else:
                messages.error(request, 'Invalid username or password.')
        else:
            messages.error(request, 'Invalid username or password.')
    form = AuthenticationForm()
    return render(request, 'task/login.html', {'form': form})


def logout_view(request):
    logout(request)
    return redirect('login')


def leaderboard_view(request):
    leaderboard = (Attempt.objects.filter(correct=True)
                   .values('user__username', 'user__profile__color')
                   .annotate(score=Sum('task__points'))
                   .order_by('-score'))

    return render(request, 'task/leaderboard.html', {'leaderboard': leaderboard})


def task_list_view(request):
    tasks = Task.objects.all()
    return render(request, 'task/task_list.html', {'tasks': tasks})


@login_required(redirect_field_name='next', login_url='/login/')
def task_detail_view(request, task_id):
    task = get_object_or_404(Task, id=task_id)
    user = request.user

    if request.method == 'POST':
        user_answer = request.POST.get('answer')
        correct = user_answer == task.answer
        attempt_counter.inc()

        attempt, created = Attempt.objects.get_or_create(user=user, task=task)

        if correct:
            attempt.correct = True
            attempt.save()

            completed_attempts = Attempt.objects.filter(user=request.user, correct=True).select_related('task')
            total_points = completed_attempts.aggregate(Sum('task__points'))['task__points__sum'] or 0
            casino_set_json = {'secret': settings.SECRET_KEY, 'subj': user.username, 'value': total_points}
            casino_set_url = f"http://{settings.CASINO_HOST}/api/set"

            response = requests.post(casino_set_url, json=casino_set_json)
            response.raise_for_status()

            messages.success(request,
                             f'И это правильный ответ! На ваш счет зачислено {task.points} баллов!\nКрутите барабан!')
        elif not attempt.correct:
            attempt.correct = False
            attempt.save()
            messages.error(request, 'Неправильный ответ, попробуйте еще раз.')
        else:
            messages.success(request, 'Вы уже правильно ответили на эту задачу!')

        return redirect('task_detail', task_id=task.id)

    return render(request, 'task/task_detail.html', {'task': task})


def rules_view(request):
    return render(request, 'task/rules.html')


@login_required
def profile_view(request):
    user = request.user
    completed_attempts = Attempt.objects.filter(user=request.user, correct=True).select_related('task')
    total_points = completed_attempts.aggregate(Sum('task__points'))['task__points__sum'] or 0
    completed_tasks = [attempt.task for attempt in completed_attempts]

    token = get_user_token(request, request.user.username)
    casino_url = f"{settings.CASINO_HOST}/auth/page/jwt?token={token}"
    return render(request, 'task/profile.html', {
        'completed_tasks': completed_tasks,
        'total_points': total_points,
        'casino_url': casino_url,
    })


@login_required
def edit_profile_view(request):
    user = request.user
    if request.method == 'POST':
        form = ProfileForm(request.POST, color_choices=get_color_choices(request))
        if form.is_valid():
            color = form.cleaned_data['color']
            user.profile.color = color
            user.profile.save()
            messages.success(request, 'Цвет профиля успешно обновлен!')
            return redirect('profile')
    else:
        form = ProfileForm(color_choices=get_color_choices(request))

    return render(request, 'task/edit_profile.html', {'form': form, 'user_color': user.profile.color})


def get_color_choices(request):
    token = get_user_token(request, request.user.username)
    casino_get_url = f"http://{settings.CASINO_HOST}/api/rewards?token={token}"
    try:
        response = requests.get(casino_get_url)
        response.raise_for_status()
        rewards = response.json()
        return [reward['rewardValue'] for reward in rewards if reward['rewardType'] == 'Color']
    except requests.exceptions.RequestException as e:
        messages.error(request, f'Ошибка при получении списка цветов: {e}')
        return []
