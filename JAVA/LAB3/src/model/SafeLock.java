package model;

import java.util.*;

public class SafeLock extends LockDevice {

    private List<Integer> combination = Arrays.asList(0,0,0,0);
    private int wrongAttempts = 0;
    private boolean lockout = false;              // блокировка после 3 ошибок
    private boolean combinationValidated = false; // разрешение на ОДНО последующее открытие
    private double innerTempC = 22.0;             // демонстрация «случайного» свойства

    public SafeLock(String manufacturer, String model, String material, String masterKey) {
        super(manufacturer, model, material, masterKey);
        this.locked = true;
    }

    public void setCombination(List<Integer> comb) {
        if (comb == null || comb.size() != 4) return;
        for (int d : comb) if (d < 0 || d > 9) return;
        this.combination = new ArrayList<>(comb);
        this.wrongAttempts = 0;
        this.lockout = false;
        this.combinationValidated = false;
    }

    /** Ввод комбинации. При успехе разрешаем одно открытие. */
    public boolean enterCombination(List<Integer> comb) {
        if (lockout) return false;
        boolean ok = comb != null && comb.size() == 4 && comb.equals(combination);
        if (ok) {
            wrongAttempts = 0;
            combinationValidated = true;
        } else {
            wrongAttempts++;
            combinationValidated = false;
            if (wrongAttempts >= 3) lockout = true;
        }
        return ok;
    }

    /** Админ снимает блокировку. Разрешение на открытие не даём. */
    public void adminResetLockout() {
        wrongAttempts = 0;
        lockout = false;
        combinationValidated = false;
    }

    /** Закрывать можно всегда. При закрытии сбрасываем «разрешение». */
    @Override
    public void lock() {
        super.lock();
        combinationValidated = false;
    }

    /** Открыть можно только после успешного ввода комбинации и если нет блокировки. */
    @Override
    public void unlock() {
        if (!lockout && combinationValidated) {
            super.unlock();
            combinationValidated = false; // одноразовый пропуск
        }
        // иначе ничего не делаем: останется заперт
    }

    @Override
    protected void onRandomize() {
        innerTempC = Math.max(10, Math.min(50, innerTempC + (rnd.nextDouble() - 0.5) * 4));
    }

    @Override
    public String statusInfo() {
        return super.statusInfo() +
                String.format(" | Safe: попыток неверных: %d | Блокировка: %s | Tвнутр: %.1f°C | Комбинация: ****",
                        wrongAttempts, lockout ? "да" : "нет", innerTempC);
    }
}